/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package services

import connectors.EventReportConnector
import models.enumeration.ApiType._
import models.enumeration.EventType
import models.enumeration.EventType.{Event1, Event20A, Event22, WindUp}
import models.{EROverview, EROverviewVersion, ERVersion, EventDataIdentifier}
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito._
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.http.Status.NO_CONTENT
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.json._
import play.api.mvc.RequestHeader
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.{Application, inject}
import repositories.EventReportCacheRepository
import uk.gov.hmrc.http._
import utils.{GeneratorAPI1828, GeneratorAPI1829, JSONSchemaValidator, JsonFileReader}

import java.time.LocalDate
import scala.concurrent.Future
import scala.util.{Failure, Success}

class EventReportServiceSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfterEach
                                                   with JsonFileReader with GeneratorAPI1828 with GeneratorAPI1829 {

  override def generateUserAnswersAndPOSTBody: Gen[(JsObject, JsObject)] = super[GeneratorAPI1828].generateUserAnswersAndPOSTBody

  import EventReportServiceSpec._

  private implicit val hc: HeaderCarrier = HeaderCarrier()
  private implicit lazy val rh: RequestHeader = FakeRequest("", "")

  private val mockEventReportConnector = mock[EventReportConnector]
  private val mockJSONPayloadSchemaValidator = mock[JSONSchemaValidator]
  private val mockEventReportCacheRepository = mock[EventReportCacheRepository]

  private val psaId = "psa"
  private val pstr = "pstr"
  private val startDate = "startDate"
  private val payload = Json.obj("test" -> "test")
  private val year = 2020
  private val version = 1

  val modules: Seq[GuiceableModule] =
    Seq(
      inject.bind[EventReportConnector].toInstance(mockEventReportConnector),
      inject.bind[EventReportCacheRepository].toInstance(mockEventReportCacheRepository),
      inject.bind[JSONSchemaValidator].toInstance(mockJSONPayloadSchemaValidator)
    )

  val application: Application = new GuiceApplicationBuilder()
    .configure(conf = "auditing.enabled" -> false, "metrics.enabled" -> false, "metrics.jvm" -> false).
    overrides(modules: _*).build()

  private def eventReportService = application.injector.instanceOf[EventReportService]

  override def beforeEach(): Unit = {
    reset(mockEventReportConnector)
    reset(mockEventReportCacheRepository)
    reset(mockJSONPayloadSchemaValidator)
    when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any())).thenReturn(Success(()))
  }

  "compileEventReport for unimplemented api type" must {
    "return Bad Request" in {
      eventReportService.compileEventReport(psaId, "pstr", Event20A, year, version)(implicitly, implicitly, implicitly).map {
        result => result.header.status mustBe BAD_REQUEST
      }
    }
  }

  "compileEventReport for event 1" must {
    "return NOT FOUND when no data return from repository" in {
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(Some(EventDataIdentifier(Api1827, 2020, 1))))(any()))
        .thenReturn(Future.successful(None))
      eventReportService.compileEventReport(psaId, "pstr", Event1, year, version)(implicitly, implicitly, implicitly).map {
        result => result.header.status mustBe NOT_FOUND
      }
    }

    "return 204 No Content when valid data return from repository - event 1" in {
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(Some(EventDataIdentifier(Api1827, 2020, 1))))(any()))
        .thenReturn(Future.successful(Some(responseJson)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))

      when(mockEventReportConnector.compileEventOneReport(any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, responseJson.toString)))

      eventReportService.compileEventReport(psaId, "pstr", Event1, year, version).map {
        result => result.header.status mustBe NO_CONTENT
      }
    }

    "return 400 when validation errors response for event one report" in {
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(Some(EventDataIdentifier(Api1827, 2020, 1))))(any()))
        .thenReturn(Future.successful(Some(responseJson)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), eqTo(compileEventOneReportSchemaPath), any()))
        .thenReturn(Failure(new Exception("Message")))

      recoverToExceptionIf[Exception] {
        eventReportService.compileEventReport(psaId, "pstr", Event1, year, version)
      } map {
        failure =>
          failure.getMessage mustBe "Message"
      }
    }

    "throw Upstream5XXResponse on Internal Server Error" in {
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(Some(EventDataIdentifier(Api1827, 2020, 1))))(any()))
        .thenReturn(Future.successful(Some(responseJson)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))

      when(mockEventReportConnector.compileEventOneReport(any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))

      recoverToExceptionIf[UpstreamErrorResponse] {
        eventReportService.compileEventReport(psaId, "pstr", Event1, year, version)
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }
  }

  "compileEventReport for event windup" must {
    "return Not Found when no data returned from repository" in {
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(Some(EventDataIdentifier(Api1826, 2020, 1))))(any()))
        .thenReturn(Future.successful(None))
      eventReportService.compileEventReport(psaId, "pstr", WindUp, year, version)(implicitly, implicitly, implicitly).map {
        result => result.header.status mustBe NOT_FOUND
      }
    }

    "return 204 No Content when valid data return from repository - event 1" in {

      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(Some(EventDataIdentifier(Api1826, 2020, 1))))(any()))
        .thenReturn(Future.successful(Some(uaJsonEventWindUp)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))

      when(mockEventReportConnector.compileEventReportSummary(any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, responseJson.toString)))

      eventReportService.compileEventReport(psaId, "pstr", WindUp, year, version).map {
        result => result.header.status mustBe NO_CONTENT
      }
    }

    "return 400 when validation errors response" in {
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(Some(EventDataIdentifier(Api1826, 2020, 1))))(any()))
        .thenReturn(Future.successful(Some(uaJsonEventWindUp)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))

      when(mockJSONPayloadSchemaValidator.validatePayload(any(), eqTo(createCompiledEventSummaryReportSchemaPath), any()))
        .thenReturn(Failure(new Exception("Message")))

      recoverToExceptionIf[Exception] {
        eventReportService.compileEventReport(psaId, "pstr", WindUp, year, version)
      } map {
        failure =>
          failure.getMessage mustBe "Message"
      }
    }

    "throw Upstream5XXResponse on Internal Server Error" in {

      when(mockEventReportCacheRepository.getUserAnswers(any(), any())(any()))
        .thenReturn(Future.successful(Some(uaJsonEventWindUp)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))

      when(mockEventReportConnector.compileEventReportSummary(any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))

      recoverToExceptionIf[UpstreamErrorResponse] {
        eventReportService.compileEventReport(psaId, "pstr", WindUp, year, version)
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }
  }

  "getEvent" must {
    "return OK if no data was found in the cache to begin with" in {
      when(mockEventReportConnector.getEvent(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(version),
        ArgumentMatchers.eq(Some(Event22)))(any(), any()))
        .thenReturn(Future.successful(getEvent22PayLoadData))

      eventReportService.getEvent(pstr, startDate, version, Event22)(implicitly, implicitly).map { resultJsValue =>
        verify(mockEventReportConnector, times(1)).getEvent(any(), any(), any(), any())(any(), any())
        resultJsValue mustBe Some(Json.toJson(getEvent22UAData))
      }
    }
  }

  "getEventSummary" must {
    "return the payload from the connector for API1834" in {
      val response = Set("1", "2", "3", "4", "5", "6", "7", "8", "8A", "10", "11", "12", "13", "14", "18", "19", "20", "22", "23", "24", "0")
      when(mockEventReportConnector.getEvent(pstr, startDate, version, None)(implicitly, implicitly))
        .thenReturn(Future.successful(responseJsonForAPI1834))
      eventReportService.getEventSummary(pstr, version, startDate).map { result =>
        verify(mockEventReportConnector, times(1)).getEvent(pstr, startDate, version, None)(implicitly, implicitly)
        result.value.map(_.validate[String].get).toSet mustBe response
      }
    }
  }

  "saveEventToMongo" must {
    "return the payload from the connector when valid event type" in {
      when(mockEventReportCacheRepository.upsert(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(EventDataIdentifier(Api1830, year, version)),
        any()
      )(any()))
        .thenReturn(Future.successful((): Unit))

      eventReportService.saveUserAnswers(pstr, EventType.Event3, year, version, payload)(implicitly).map { result =>
        verify(mockEventReportCacheRepository, times(1)).upsert(ArgumentMatchers.eq(pstr),
          ArgumentMatchers.eq(EventDataIdentifier(Api1830, year, version)), any())(any())
        result mustBe()
      }
    }
  }

  "removeUserAnswers" must {
    "return unit when data deleted" in {
      when(mockEventReportCacheRepository.removeAllOnSignOut(
        ArgumentMatchers.eq(pstr)
      )(any()))
        .thenReturn(Future.successful((): Unit))
      eventReportService.removeUserAnswers(pstr)(implicitly).map { result =>
        verify(mockEventReportCacheRepository, times(1)).removeAllOnSignOut(ArgumentMatchers.eq(pstr))(any())
        result mustBe()
      }
    }
  }

  "getUserAnswers with event type" must {
    "return the payload from the connector when valid event type" in {
      val json = Json.obj("test" -> "test")
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(Some(EventDataIdentifier(Api1830, 2020, 1))))(any()))
        .thenReturn(Future.successful(Some(json)))

      eventReportService.getUserAnswers(pstr, EventType.Event3, year, version)(implicitly, implicitly).map { result =>
        result mustBe Some(json)
      }
    }
  }

  "getUserAnswers with NO event type" must {
    "return the payload from the connector when valid event type" in {
      val json = Json.obj("test" -> "test")
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(pstr), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(json)))

      eventReportService.getUserAnswers(pstr)(implicitly).map { result =>
        result mustBe Some(json)
      }
    }
  }

  "getVersions" must {
    "return the payload from the connector when valid event type" in {
      when(mockEventReportConnector.getVersions(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq("ER"),
        ArgumentMatchers.eq(startDate))(any(), any()))
        .thenReturn(Future.successful(erVersions))

      when(mockEventReportConnector.getVersions(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq("ER20A"),
        ArgumentMatchers.eq(startDate))(any(), any()))
        .thenReturn(Future.successful(erVersionsER20A))

      eventReportService.getVersions(pstr, startDate)(implicitly, implicitly).map { result =>
        result mustBe erVersions ++ erVersionsER20A
      }
    }
  }

  "getOverview" must {
    "return OK with the Seq of overview details and save the data in cache if no data was found in the cache to begin with" in {
      when(mockEventReportConnector.getOverview(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq("ER"),
        ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(endDate))(any(), any()))
        .thenReturn(Future.successful(erOverview))

      when(mockEventReportConnector.getOverview(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq("ER20A"),
        ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(endDate))(any(), any()))
        .thenReturn(Future.successful(er20AOverview))

      eventReportService.getOverview(pstr, startDate, endDate)(implicitly, implicitly).map { resultJsValue =>

        verify(mockEventReportConnector, times(2)).getOverview(any(), any(), any(), any())(any(), any())

        resultJsValue mustBe Json.toJson(erOverview ++ er20AOverview)
      }
    }
  }

  "submitEventDeclarationReport" must {
    "return valid response where there are changes" in {
      val (userAnswers, submitEventDeclarationReportSuccessResponseETMP) = super[GeneratorAPI1828].generateUserAnswersAndPOSTBody.sample.value
      when(mockEventReportConnector.submitEventDeclarationReport(
        ArgumentMatchers.eq(pstr),
        any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))
      eventReportService.submitEventDeclarationReport(pstr, userAnswers)(implicitly, implicitly, implicitly).map { _ =>
        verify(mockEventReportConnector, times(1)).submitEventDeclarationReport(ArgumentMatchers.eq(pstr),
          ArgumentMatchers.eq(submitEventDeclarationReportSuccessResponseETMP))(any(), any(), any())
        assert(true)
      }
    }
    "return valid an invalid response where an 1829 payload is passed" in {
      val userAnswers = super[GeneratorAPI1829].generateUserAnswersAndPOSTBody.sample.value._1
      when(mockEventReportConnector.submitEventDeclarationReport(
        ArgumentMatchers.eq(pstr),
        any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))
      eventReportService.submitEventDeclarationReport(pstr, userAnswers)(implicitly, implicitly, implicitly).map { _ =>
        verify(mockEventReportConnector, times(1)).submitEventDeclarationReport(ArgumentMatchers.eq(pstr),
          any())(any(), any(), any())
        assert(true)
      }
    }
    "return a 417 error response when there is nothing to submit" in {
      val (userAnswers, submitEventDeclarationReportSuccessResponseETMP) = super[GeneratorAPI1828].generateUserAnswersAndPOSTBody.sample.value
      when(mockEventReportConnector.submitEventDeclarationReport(
        ArgumentMatchers.eq(pstr),
        any())(any(), any(), any()))
        .thenReturn(Future.failed(new BadRequestException("Test")))
      recoverToExceptionIf[ExpectationFailedException] {
        eventReportService.submitEventDeclarationReport(pstr, userAnswers)(implicitly, implicitly, implicitly)
      } map { _ =>
        verify(mockEventReportConnector, times(1)).submitEventDeclarationReport(ArgumentMatchers.eq(pstr),
          ArgumentMatchers.eq(submitEventDeclarationReportSuccessResponseETMP))(any(), any(), any())
        assert(true)
      }
    }
    "return a Future failed when validation fails against the schema for API1828" in {
      val (userAnswers, submitEventDeclarationReportSuccessResponseETMP) = super[GeneratorAPI1828].generateUserAnswersAndPOSTBody.sample.value
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), eqTo(SchemaPath1828), any())).thenReturn(Failure(new Exception("Message")))
      when(mockEventReportConnector.submitEventDeclarationReport(
        ArgumentMatchers.eq(pstr),
        any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))

      recoverToExceptionIf[Exception] {
        eventReportService.submitEventDeclarationReport(pstr, userAnswers)(implicitly, implicitly, implicitly)
      } map { _ =>
        verify(mockEventReportConnector, times(1)).submitEventDeclarationReport(ArgumentMatchers.eq(pstr), any())(any(), any(), any())
        verify(mockJSONPayloadSchemaValidator, times(1)).validatePayload(ArgumentMatchers.eq(submitEventDeclarationReportSuccessResponseETMP),
          ArgumentMatchers.eq(SchemaPath1828), ArgumentMatchers.eq("submitEventDeclarationReport"))
        assert(true)
      }
    }
  }

  "submitEvent20ADeclarationReport" must {
    "return valid response where there are changes" in {
      val (userAnswers, submitEvent20ADeclarationReportSuccessResponseETMP) = super[GeneratorAPI1829].generateUserAnswersAndPOSTBody.sample.value
      when(mockEventReportConnector.submitEvent20ADeclarationReport(
        ArgumentMatchers.eq(pstr),
        any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))
      eventReportService.submitEvent20ADeclarationReport(pstr, userAnswers)(implicitly, implicitly, implicitly).map { _ =>
        verify(mockEventReportConnector, times(1)).submitEvent20ADeclarationReport(ArgumentMatchers.eq(pstr),
          ArgumentMatchers.eq(submitEvent20ADeclarationReportSuccessResponseETMP))(any(), any(), any())
        assert(true)
      }
    }
    "return a 417 error response when there is nothing to submit" in {
      val (userAnswers, submitEvent20ADeclarationReportSuccessResponseETMP) = super[GeneratorAPI1829].generateUserAnswersAndPOSTBody.sample.value
      when(mockEventReportConnector.submitEvent20ADeclarationReport(
        ArgumentMatchers.eq(pstr),
        any())(any(), any(), any()))
        .thenReturn(Future.failed(new BadRequestException("Test")))
      recoverToExceptionIf[ExpectationFailedException] {
        eventReportService.submitEvent20ADeclarationReport(pstr, userAnswers)(implicitly, implicitly, implicitly)
      } map { _ =>
        verify(mockEventReportConnector, times(1)).submitEvent20ADeclarationReport(ArgumentMatchers.eq(pstr),
          ArgumentMatchers.eq(submitEvent20ADeclarationReportSuccessResponseETMP))(any(), any(), any())
        assert(true)
      }
    }
    "return a Future failed when validation fails against the schema for API1829" in {
      val (userAnswers, submitEvent20ADeclarationReportSuccessResponseETMP) = super[GeneratorAPI1829].generateUserAnswersAndPOSTBody.sample.value
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), eqTo(SchemaPath1829), any())).thenReturn(Failure(new Exception("Message")))
      when(mockEventReportConnector.submitEvent20ADeclarationReport(
        ArgumentMatchers.eq(pstr),
        any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))
      recoverToExceptionIf[Exception] {
        eventReportService.submitEvent20ADeclarationReport(pstr, userAnswers)(implicitly, implicitly, implicitly)
      } map { _ =>
        verify(mockEventReportConnector, times(1)).submitEvent20ADeclarationReport(ArgumentMatchers.eq(pstr), any())(any(), any(), any())
        verify(mockJSONPayloadSchemaValidator, times(1)).validatePayload(ArgumentMatchers.eq(submitEvent20ADeclarationReportSuccessResponseETMP),
          ArgumentMatchers.eq(SchemaPath1829), ArgumentMatchers.eq("submitEvent20ADeclarationReport"))
        assert(true)
      }
    }
    "return valid an invalid response where an 1828 payload is passed" in {
      val userAnswers = super[GeneratorAPI1828].generateUserAnswersAndPOSTBody.sample.value._1
      when(mockEventReportConnector.submitEvent20ADeclarationReport(
        ArgumentMatchers.eq(pstr),
        any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))
      eventReportService.submitEvent20ADeclarationReport(pstr, userAnswers)(implicitly, implicitly, implicitly).map { _ =>
        verify(mockEventReportConnector, times(1)).submitEvent20ADeclarationReport(ArgumentMatchers.eq(pstr),
          any())(any(), any(), any())
        assert(true)
      }
    }
  }
}

object EventReportServiceSpec {
  private val responseJson: JsObject = Json.obj("event" -> "mockEvent - test passed")
  private val responseNoEventTypeJson: JsObject = Json.obj("taxYear" -> "2022")
  private val uaJsonEventWindUp: JsObject =
    Json.obj(
      "schemeWindUpDate" -> "2020-06-01"
    )
  private val createCompiledEventSummaryReportSchemaPath = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  private val compileEventOneReportSchemaPath = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.4.json"

  private final val SchemaPath1828 = "/resources.schemas/api-1828-submit-event-declaration-report-request-schema-v1.0.4.json"
  private final val SchemaPath1829 = "/resources.schemas/api-1829-submit-event20a-declaration-report-request-schema-v1.0.0.json"

  private val endDate = "2023-04-05"

  private val version = ERVersion(1,
    LocalDate.of(2022, 4, 6),
    "Compiled")

  private val erVersions = Seq(version)

  private val versionER20A = ERVersion(2,
    LocalDate.of(2022, 6, 4),
    "Compiled")

  private val erVersionsER20A = Seq(versionER20A)

  private val overview1 = EROverview(
    LocalDate.of(2022, 4, 6),
    LocalDate.of(2023, 4, 5),
    tpssReportPresent = false,
    Some(EROverviewVersion(
      3,
      submittedVersionAvailable = false,
      compiledVersionAvailable = true)))

  private val overview2 = EROverview(
    LocalDate.of(2022, 4, 6),
    LocalDate.of(2023, 4, 5),
    tpssReportPresent = false,
    Some(EROverviewVersion(
      2,
      submittedVersionAvailable = true,
      compiledVersionAvailable = true)))

  private val getEvent22PayLoadData: Option[JsObject] = Some(Json.parse(
    """
      |    {
      |    "eventDetails": [
      |    {
      |    "memberDetail": {
      |        "event": {
      |          "eventType": "Event22",
      |          "individualDetails": {
      |            "title": "Mr",
      |            "firstName": "John",
      |            "middleName": "A",
      |            "lastName": "Smith",
      |            "nino": "AA345678B"
      |          },
      |          "paymentDetails": {
      |            "monetaryAmount": 123.99,
      |            "taxYearEndingDate": "2022-05-30"
      |          }
      |        }
      |       }
      |      }
      |    ]
      |} """.stripMargin
  ).as[JsObject])

  private val getEvent22UAData: Option[JsValue] = Some(Json.parse(
    """
      |{
      |    "event22": {
      |      "members": [
      |        {
      |          "membersDetails": {
      |            "firstName": "John",
      |            "lastName": "Smith",
      |            "nino": "AA345678B"
      |          },
      |          "chooseTaxYear": "2021",
      |          "totalPensionAmounts": 123.99
      |        }
      |      ]
      |    }
      |} """.stripMargin
  ))

  private val erOverview = Seq(overview1, overview2)
  private val er20AOverview = Seq(overview1, overview2)
  private val responseJsonForAPI1834: Option[JsObject] = Some(Json.parse(
    """
      |{
      |    "schemeDetails": {
      |        "pSTR": "87219363YN",
      |        "schemeName": "Abc Ltd"
      |    },
      |    "eventReportDetails": {
      |        "reportFormBundleNumber": "123456789012",
      |        "reportStartDate": "2021-04-06",
      |        "reportEndDate": "2022-04-05",
      |        "reportStatus": "Compiled",
      |        "reportVersionNumber": "001",
      |        "reportSubmittedDateAndTime": "2023-12-13T12:12:12Z"
      |    },
      |    "eventDetails": {
      |        "event10": [
      |            {
      |                "recordVersion": "001",
      |                "invRegScheme": {
      |                    "startDateDetails": {
      |                        "startDateOfInvReg": "2022-01-31",
      |                        "contractsOrPolicies": "Yes"
      |                    }
      |                }
      |            }
      |        ],
      |        "event11": {
      |            "recordVersion": "001",
      |            "unauthorisedPmtsDate": "2022-01-31",
      |            "contractsOrPoliciesDate": "2022-01-10"
      |        },
      |        "event12": {
      |            "recordVersion": "001",
      |            "twoOrMoreSchemesDate": "2022-01-02"
      |        },
      |        "event13": [
      |            {
      |                "recordVersion": "001",
      |                "schemeStructure": "A single trust under which all of the assets are held for the benefit of all members of the scheme",
      |                "dateOfChange": "2022-01-02"
      |            }
      |        ],
      |        "event14": {
      |            "recordVersion": "001",
      |            "schemeMembers": "12 to 50"
      |        },
      |        "event18": {
      |            "recordVersion": "001",
      |            "chargeablePmt": "Yes"
      |        },
      |        "event19": [
      |            {
      |                "recordVersion": "001",
      |                "countryCode": "GB",
      |                "dateOfChange": "2022-01-14"
      |            }
      |        ],
      |        "event20": [
      |            {
      |                "recordVersion": "001",
      |                "occSchemeDetails": {
      |                    "startDateOfOccScheme": "2022-01-27"
      |                }
      |            }
      |        ],
      |        "eventWindUp": {
      |            "recordVersion": "001",
      |            "dateOfWindUp": "2022-01-28"
      |        }
      |    },
      |    "memberEventsSummary": {
      |        "event2": {
      |            "recordVersion": "001",
      |            "numberOfMembers": 150000
      |        },
      |        "event3": {
      |            "recordVersion": "002",
      |            "numberOfMembers": 1000
      |        },
      |        "event4": {
      |            "recordVersion": "001",
      |            "numberOfMembers": 1000
      |        },
      |        "event5": {
      |            "recordVersion": "004",
      |            "numberOfMembers": 50000
      |        },
      |        "event6": {
      |            "recordVersion": "007",
      |            "numberOfMembers": 10000
      |        },
      |        "event7": {
      |            "recordVersion": "002",
      |            "numberOfMembers": 150000
      |        },
      |        "event8": {
      |            "recordVersion": "004",
      |            "numberOfMembers": 1500
      |        },
      |        "event8A": {
      |            "recordVersion": "003",
      |            "numberOfMembers": 150000
      |        },
      |        "event22": {
      |            "recordVersion": "004",
      |            "numberOfMembers": 10000
      |        },
      |        "event23": {
      |            "recordVersion": "003",
      |            "numberOfMembers": 150000
      |        },
      |        "event24": {
      |            "recordVersion": "001",
      |            "numberOfMembers": 150000
      |        }
      |    },
      |    "event1ChargeDetails": {
      |        "recordVersion": "002",
      |        "numberOfMembers": 1000,
      |        "sscCharge": {
      |            "totalCharge": 10000.66,
      |            "previousPostedCharge": 67000,
      |            "deltaCharge": 8099.78,
      |            "chargeReference": "0123456789012345"
      |        },
      |        "fcmtCharge": {
      |            "totalCharge": 107889.66,
      |            "previousPostedCharge": 123456,
      |            "deltaCharge": 1299.78,
      |            "chargeReference": "0123456789012677"
      |        }
      |    }
      |}
      |""".stripMargin).as[JsObject])
}


