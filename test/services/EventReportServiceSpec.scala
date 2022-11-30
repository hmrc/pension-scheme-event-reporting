/*
 * Copyright 2022 HM Revenue & Customs
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
import models.enumeration.EventType.{Event1, Event20A, WindUp}
import models.enumeration.{ApiType, EventType}
import models.{EROverview, EROverviewVersion, ERVersion}
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.{any, eq => eqTo}
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures.whenReady
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.http.Status.NO_CONTENT
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.json.{JsArray, JsObject, Json}
import play.api.test.Helpers._
import play.api.{Application, inject}
import repositories.{EventReportCacheRepository, OverviewCacheRepository}
import uk.gov.hmrc.http._
import utils.JSONSchemaValidator

import java.time.LocalDate
import scala.concurrent.Future
import scala.util.{Failure, Success}

class EventReportServiceSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfterEach {

  import EventReportServiceSpec._

  private implicit val hc: HeaderCarrier = HeaderCarrier()
  private val mockEventReportConnector = mock[EventReportConnector]
  private val mockJSONPayloadSchemaValidator = mock[JSONSchemaValidator]
  private val mockEventReportCacheRepository = mock[EventReportCacheRepository]
  private val mockOverviewCacheRepository = mock[OverviewCacheRepository]

  private val pstr = "pstr"
  private val startDate = "startDate"
  private val version = "version"
  private val payload = Json.obj("test" -> "test")

  val modules: Seq[GuiceableModule] =
    Seq(
      inject.bind[EventReportConnector].toInstance(mockEventReportConnector),
      inject.bind[EventReportCacheRepository].toInstance(mockEventReportCacheRepository),
      inject.bind[JSONSchemaValidator].toInstance(mockJSONPayloadSchemaValidator),
      inject.bind[OverviewCacheRepository].toInstance(mockOverviewCacheRepository)
    )

  val application: Application = new GuiceApplicationBuilder()
    .configure(conf = "auditing.enabled" -> false, "metrics.enabled" -> false, "metrics.jvm" -> false).
    overrides(modules: _*).build()

  private def eventReportService = application.injector.instanceOf[EventReportService]

  override def beforeEach(): Unit = {
    reset(mockEventReportConnector)
    reset(mockOverviewCacheRepository)
    reset(mockEventReportCacheRepository)
    reset(mockJSONPayloadSchemaValidator)
    when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any())).thenReturn(Success(()))
    when(mockOverviewCacheRepository.get(any(), any(), any(), any())(any())).thenReturn(Future.successful(None))
  }

  "compileEventReport for unimplemented api type" must {
    "return Bad Request" in {
      eventReportService.compileEventReport("pstr", Event20A)(implicitly, implicitly).map {
        result => result.header.status mustBe BAD_REQUEST
      }
    }
  }

  "compileEventReport for event 1" must {
    "return NOT FOUND when no data return from repository" in {
      when(mockEventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1827.toString))(implicitly))
        .thenReturn(Future.successful(None))
      eventReportService.compileEventReport("pstr", Event1)(implicitly, implicitly).map {
        result => result.header.status mustBe NOT_FOUND
      }
    }

    "return 204 No Content when valid data return from repository - event 1" in {

      when(mockEventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1827.toString))(implicitly))
        .thenReturn(Future.successful(Some(responseJson)))

      when(mockEventReportConnector.compileEventOneReport(any(), any())(any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, responseJson.toString)))

      eventReportService.compileEventReport("pstr", Event1).map {
        result => result.header.status mustBe NO_CONTENT
      }
    }

    "return 400 when validation errors response for event one report" in {
      when(mockEventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1827.toString))(implicitly))
        .thenReturn(Future.successful(Some(responseJson)))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), eqTo(compileEventOneReportSchemaPath), any()))
        .thenReturn(Failure(new Exception("Message")))

      recoverToExceptionIf[Exception] {
        eventReportService.compileEventReport("pstr", Event1)
      } map {
        failure =>
          failure.getMessage mustBe "Message"
      }
    }

    "throw Upstream5XXResponse on Internal Server Error" in {
      when(mockEventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1827.toString))(implicitly))
        .thenReturn(Future.successful(Some(responseJson)))

      when(mockEventReportConnector.compileEventOneReport(any(), any())(any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))

      recoverToExceptionIf[UpstreamErrorResponse] {
        eventReportService.compileEventReport("pstr", Event1)
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }
  }

  "compileEventReport for event windup" must {
    "return Not Found when no data returned from repository" in {
      when(mockEventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1826.toString))(implicitly))
        .thenReturn(Future.successful(None))
      eventReportService.compileEventReport("pstr", WindUp)(implicitly, implicitly).map {
        result => result.header.status mustBe NOT_FOUND
      }
    }

    "return Not Found when data return from repository but nothing to transform" in {
      when(mockEventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1826.toString))(implicitly))
        .thenReturn(Future.successful(Some(Json.obj())))
      eventReportService.compileEventReport("pstr", WindUp)(implicitly, implicitly).map {
        result => result.header.status mustBe NOT_FOUND
      }
    }

    "return 204 No Content when valid data return from repository - event 1" in {
      when(mockEventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1826.toString))(implicitly))
        .thenReturn(Future.successful(Some(uaJsonEventWindUp)))

      when(mockEventReportConnector.compileEventReportSummary(any(), any())(any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, responseJson.toString)))

      eventReportService.compileEventReport("pstr", WindUp).map {
        result => result.header.status mustBe NO_CONTENT
      }
    }

    "return 400 when validation errors response" in {
      when(mockEventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1826.toString))(implicitly))
        .thenReturn(Future.successful(Some(uaJsonEventWindUp)))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), eqTo(createCompiledEventSummaryReportSchemaPath), any()))
        .thenReturn(Failure(new Exception("Message")))

      recoverToExceptionIf[Exception] {
        eventReportService.compileEventReport("pstr", WindUp)
      } map {
        failure =>
          failure.getMessage mustBe "Message"
      }
    }

    "throw Upstream5XXResponse on Internal Server Error" in {
      when(mockEventReportCacheRepository.getByKeys(any())(any()))
        .thenReturn(Future.successful(Some(uaJsonEventWindUp)))

      when(mockEventReportConnector.compileEventReportSummary(any(), any())(any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))

      recoverToExceptionIf[UpstreamErrorResponse] {
        eventReportService.compileEventReport("pstr", WindUp)
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }
  }


  "getEvent" must {
    "return the payload from the connector when a valid event type is supplied for Api1832 and connector returns no data" in {
      when(mockEventReportConnector.getEvent(pstr, startDate, version, Some(EventType.Event3))(implicitly, implicitly))
        .thenReturn(Future.successful(None))
      whenReady(eventReportService.getEvent(pstr, startDate, version, EventType.Event3)(implicitly, implicitly)) { result =>
        result mustBe None
      }
    }
    "return the payload from the connector when a valid event type is supplied for Api1832" in {
      when(mockEventReportConnector.getEvent(pstr, startDate, version, Some(EventType.Event3))(implicitly, implicitly))
        .thenReturn(Future.successful(Some(responseJson)))
      whenReady(eventReportService.getEvent(pstr, startDate, version, EventType.Event3)(implicitly, implicitly)) { result =>
        result mustBe Some(responseJson)
      }
    }

    "return the payload from the connector when a valid event type is supplied for API1833" in {
      when(mockEventReportConnector.getEvent(pstr, startDate, version, Some(EventType.Event1))(implicitly, implicitly))
        .thenReturn(Future.successful(Some(responseJson)))
      whenReady(eventReportService.getEvent(pstr, startDate, version, EventType.Event1)(implicitly, implicitly)) { result =>
        result mustBe Some(responseJson)

      }
    }

    "return the payload from the connector when a valid event type is supplied for API1834" in {
      when(mockEventReportConnector.getEvent(pstr, startDate, version, Some(EventType.Event10))(implicitly, implicitly))
        .thenReturn(Future.successful(Some(responseJson)))
      whenReady(eventReportService.getEvent(pstr, startDate, version, EventType.Event10)) { result =>
        result mustBe Some(responseJson)
      }
    }


    "return the payload from the connector when a valid event type is supplied for API1831" in {
      when(mockEventReportConnector.getEvent(pstr, startDate, version, Some(EventType.Event20A))(implicitly, implicitly))
        .thenReturn(Future.successful(Some(responseJson)))
      whenReady(eventReportService.getEvent(pstr, startDate, version, EventType.Event20A)(implicitly, implicitly)) { result =>
        result mustBe Some(responseJson)
      }
    }
  }

  "getEventSummary" must {
    "return the payload from the connector for API1832(Event22, Event23) and API1834" in {
      val responseJson = Some(JsArray())
      when(mockEventReportConnector.getEvent(pstr, startDate, version, Some(EventType.Event22))(implicitly, implicitly))
        .thenReturn(Future.successful(responseJson))
      when(mockEventReportConnector.getEvent(pstr, startDate, version, Some(EventType.Event23))(implicitly, implicitly))
        .thenReturn(Future.successful(responseJson))
      when(mockEventReportConnector.getEvent(pstr, startDate, version, None)(implicitly, implicitly))
        .thenReturn(Future.successful(responseJson))
      eventReportService.getEventSummary(pstr, version, startDate).map { result =>
        verify(mockEventReportConnector, times(1)).getEvent(pstr, startDate, version, Some(EventType.Event22))(implicitly, implicitly)
        verify(mockEventReportConnector, times(1)).getEvent(pstr, startDate, version, Some(EventType.Event23))(implicitly, implicitly)
        verify(mockEventReportConnector, times(1)).getEvent(pstr, startDate, version, None)(implicitly, implicitly)
        result mustBe responseJson.get
      }
    }
  }

  "saveEventToMongo" must {
    "return the payload from the connector when valid event type" in {
      when(mockEventReportCacheRepository.upsert(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(Api1830),
        any()
      )(any()))
        .thenReturn(Future.successful((): Unit))
      eventReportService.saveUserAnswers(pstr, EventType.Event3, payload)(implicitly).map { result =>
        verify(mockEventReportCacheRepository, times(1)).upsert(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(Api1830), any())(any())
        result mustBe ()
      }
    }
  }

  "getEventFromMongo" must {
    "return the payload from the connector when valid event type" in {
      val json = Json.obj("test" -> "test")

      val mapOfKeys = Map(
        "pstr" -> pstr,
        "apiTypes" -> ApiType.Api1830.toString
      )
      when(mockEventReportCacheRepository.getByKeys(
        ArgumentMatchers.eq(mapOfKeys)
      )(any())).thenReturn(Future.successful(Some(json)))

      eventReportService.getUserAnswers(pstr, EventType.Event3)(implicitly).map { result =>
        result mustBe Some(json)
      }
    }
  }

  "getVersions" must {
    "return the payload from the connector when valid event type" in {
      when(mockEventReportConnector.getVersions(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(reportTypeER),
        ArgumentMatchers.eq(startDate))(any(), any()))
        .thenReturn(Future.successful(erVersions))
      whenReady(eventReportService.getVersions(pstr, "ER", startDate)(implicitly, implicitly)) { result =>
        result mustBe erVersions
      }
    }
  }

  "getOverview" must {
    "return OK with the Seq of overview details and save the data in cache if no data was found in the cache to begin with" in {
      when(mockEventReportConnector.getOverview(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(reportTypeER),
        ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(endDate))(any(), any()))
        .thenReturn(Future.successful(erOverview))
      when(mockOverviewCacheRepository.get(any(), any(), any(), any())(any())).thenReturn(Future.successful(None))
      when(mockOverviewCacheRepository.upsert(any(), any(), any(), any(), any())(any())).thenReturn(Future.successful(()))

      eventReportService.getOverview(pstr, reportTypeER, startDate, endDate)(implicitly, implicitly).map { resultJsValue =>
        verify(mockOverviewCacheRepository, times(1)).get(any(), any(), any(), any())(any())
        verify(mockOverviewCacheRepository, times(1)).upsert(any(), any(), any(), any(), any())(any())
        verify(mockEventReportConnector, times(1)).getOverview(any(), any(), any(), any())(any(), any())
        resultJsValue mustBe Json.toJson(erOverview)
      }
    }

    "return OK with the Seq of overview details and don't try to save the data in cache if the data already exists in the cache" in {
      when(mockOverviewCacheRepository.get(any(), any(), any(), any())(any())).thenReturn(Future.successful(Some(Json.toJson(erOverview))))
      eventReportService.getOverview(pstr, reportTypeER, startDate, endDate)(implicitly, implicitly).map { resultJsValue =>
        verify(mockOverviewCacheRepository, times(1)).get(any(), any(), any(), any())(any())
        verify(mockOverviewCacheRepository, never).upsert(any(), any(), any(), any(), any())(any())
        verify(mockEventReportConnector, never).getOverview(any(), any(), any(), any())(any(), any())
        resultJsValue mustBe Json.toJson(erOverview)
      }
    }
  }

  "submitEventDeclarationReport" must {
    "return valid response" in {
      when(mockEventReportConnector.submitEventDeclarationReport(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(submitEventDeclarationReportSuccessResponse))(any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = submitEventDeclarationReportSuccessResponse,
          headers = Map.empty)))
      eventReportService.submitEventDeclarationReport(pstr, submitEventDeclarationReportSuccessResponse)(implicitly, implicitly).map { resultJsValue =>
        resultJsValue mustBe submitEventDeclarationReportSuccessResponse
      }
    }
  }

  "submitEvent20ADeclarationReport" must {
    "return valid response" in {
      when(mockEventReportConnector.submitEvent20ADeclarationReport(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(submitEvent20ADeclarationReportSuccessResponse))(any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = submitEvent20ADeclarationReportSuccessResponse,
          headers = Map.empty)))
      eventReportService.submitEvent20ADeclarationReport(pstr, submitEvent20ADeclarationReportSuccessResponse)(implicitly, implicitly).map { resultJsValue =>
        resultJsValue mustBe submitEvent20ADeclarationReportSuccessResponse
      }
    }
  }
}

object EventReportServiceSpec {

  private val responseJson: JsObject = Json.obj("event" -> "mockEvent - test passed")
  private val uaJsonEventWindUp: JsObject =
    Json.obj(
      "schemeWindUpDate" -> "2020-06-01"
    )
  private val createCompiledEventSummaryReportSchemaPath = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  private val compileEventOneReportSchemaPath = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.1.json"

  private val endDate = "2023-04-05"
  private val reportTypeER = "ER"

  private val version = ERVersion(1,
    LocalDate.of(2022, 4, 6),
    "Compiled")
  private val erVersions = Seq(version)

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

  private val erOverview = Seq(overview1, overview2)

  private val submitEventDeclarationReportSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678933")

  private val submitEvent20ADeclarationReportSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345670811")

}


