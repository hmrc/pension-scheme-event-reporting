/*
 * Copyright 2024 HM Revenue & Customs
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

import com.mongodb.client.result.DeleteResult
import connectors.EventReportConnector
import models.enumeration.EventType
import models.enumeration.EventType.{Event1, Event20A, Event22, Event3, Event5, Event6, WindUp}
import models.{EROverview, EROverviewVersion, EventDataIdentifier}
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
import play.api.mvc.Results.BadRequest
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.{Application, inject}
import repositories.{DeclarationLockRepository, EventLockRepository, EventReportCacheRepository}
import uk.gov.hmrc.auth.core.retrieve.Name
import uk.gov.hmrc.http._
import utils.{GeneratorAPI1828, GeneratorAPI1829, JSONSchemaValidator, JsonFileReader}

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
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
  private val mockCompilePayloadService = mock[CompilePayloadService]
  private val mockDeclarationLockRepository: DeclarationLockRepository = mock[DeclarationLockRepository]
  private val mockEventLockRepository = mock[EventLockRepository]

  private val externalId = "externalId"
  private val psaId = "psa"
  private val pstr = "pstr"
  private val reportVersion = "1"
  private val startDate = "2021-01-01"
  private val payload = Json.obj("test" -> "test")
  private val payload2 = Json.obj("test" -> "test2")
  private val year = 2020
  private val testValue = "test"
  private val testName = Name(Some("firstName"), Some("lastName"))

  val modules: Seq[GuiceableModule] =
    Seq(
      inject.bind[EventReportConnector].toInstance(mockEventReportConnector),
      inject.bind[EventReportCacheRepository].toInstance(mockEventReportCacheRepository),
      inject.bind[JSONSchemaValidator].toInstance(mockJSONPayloadSchemaValidator),
      inject.bind[CompilePayloadService].toInstance(mockCompilePayloadService),
      inject.bind[DeclarationLockRepository].toInstance(mockDeclarationLockRepository),
      inject.bind[EventLockRepository].toInstance(mockEventLockRepository)
    )

  val application: Application = new GuiceApplicationBuilder()
    .configure(conf = "auditing.enabled" -> false, "metrics.enabled" -> false, "metrics.jvm" -> false).
    overrides(modules: _*).build()

  private def eventReportService = application.injector.instanceOf[EventReportService]

  override def beforeEach(): Unit = {
    reset(mockEventReportConnector)
    reset(mockEventReportCacheRepository)
    reset(mockJSONPayloadSchemaValidator)
    reset(mockCompilePayloadService)
    reset(mockDeclarationLockRepository)
    reset(mockEventLockRepository)
    when(mockEventLockRepository.upsertIfNotLocked(any(),any(), any())).thenReturn(Future.successful(true))
    when(mockEventLockRepository.remove(any())).thenReturn(Future.successful(mock[DeleteResult]))
    when(mockEventLockRepository.eventIsLocked(any(), any(), any())).thenReturn(Future.successful(false))
    when(mockEventLockRepository.getLockedEventTypes(any(), any(), any(), any(), any())).thenReturn(Future.successful(Seq()))
    when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any())).thenReturn(Success(()))
    when(mockCompilePayloadService.collatePayloadsAndUpdateCache(any(), any(), any(), any(), any(), any(), any())(any(), any()))
      .thenReturn(Future.successful(Json.obj()))
  }

  "compileEventReport for unimplemented api type" must {
    "return Bad Request" in {
      eventReportService.compileEventReport(externalId, psaId, "pstr", Event20A, year, reportVersion, reportVersion)(implicitly, implicitly, implicitly).map {
        result => result.header.status mustBe BAD_REQUEST
      }
    }
  }

  "compileEventReport for event 1" must {
    "return NOT FOUND when no data return from repository" in {
      when(mockCompilePayloadService.addRecordVersionToUserAnswersJson(any(), any(), any()))
        .thenReturn(responseJsonEvent1WithRecordVersion)
      when(mockEventReportCacheRepository.getUserAnswers(any(), any(), any())(any()))
        .thenReturn(Future.successful(None))
      eventReportService.compileEventReport(externalId, psaId, "pstr", Event1, year, reportVersion, reportVersion)(implicitly, implicitly, implicitly).map {
        result => result.header.status mustBe NOT_FOUND
      }
    }

    "return 204 No Content when valid data return from repository - event 1" in {
      when(mockCompilePayloadService.addRecordVersionToUserAnswersJson(any(), any(), any()))
        .thenReturn(responseJsonEvent1WithRecordVersion)
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), any(), eqTo(Some(EventDataIdentifier(Event1, 2020, 1, externalId))))(any()))
        .thenReturn(Future.successful(Some(responseJsonEvent1)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), any(), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))

      when(mockEventReportConnector.compileEventOneReport(any(), any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, responseJson.toString)))

      eventReportService.compileEventReport(externalId, psaId, "pstr", Event1, year, reportVersion, reportVersion).map {
        result => result.header.status mustBe NO_CONTENT
      }
    }

    "return 400 when validation errors response for event one report" in {
      when(mockCompilePayloadService.addRecordVersionToUserAnswersJson(any(), any(), any()))
        .thenReturn(responseJsonEvent1WithRecordVersion)
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), any(), eqTo(Some(EventDataIdentifier(Event1, 2020, 1, externalId))))(any()))
        .thenReturn(Future.successful(Some(responseJsonEvent1)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), any(), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), eqTo(compileEventOneReportSchemaPath), any()))
        .thenReturn(Failure(new Exception("Message")))

      recoverToExceptionIf[Exception] {
        eventReportService.compileEventReport(externalId, psaId, "pstr", Event1, year, reportVersion, reportVersion)
      } map {
        failure =>
          failure.getMessage mustBe "Message"
      }
    }

    "throw Upstream5XXResponse on Internal Server Error" in {
      when(mockCompilePayloadService.addRecordVersionToUserAnswersJson(any(), any(), any()))
        .thenReturn(responseJsonEvent1WithRecordVersion)
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), any(), eqTo(Some(EventDataIdentifier(Event1, 2020, 1, externalId))))(any()))
        .thenReturn(Future.successful(Some(responseJsonEvent1)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), any(), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))

      when(mockEventReportConnector.compileEventOneReport(any(), any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))

      recoverToExceptionIf[UpstreamErrorResponse] {
        eventReportService.compileEventReport(externalId, psaId, "pstr", Event1, year, reportVersion, reportVersion)
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }
  }

  "compileEventReport for event windup" must {
    "return Not Found when no data returned from repository" in {
      when(mockEventReportCacheRepository.getUserAnswers(any(), any(), any())(any()))
        .thenReturn(Future.successful(None))

      val reportVersion = "2"
      val currentVersion = "1"
      when(mockCompilePayloadService.collatePayloadsAndUpdateCache(any(), any(),
        ArgumentMatchers.eq(currentVersion), ArgumentMatchers.eq(reportVersion), any(), any(), any())(any(), any()))
        .thenReturn(Future.successful(Json.obj()))

      eventReportService.compileEventReport(externalId, psaId, "pstr", WindUp, year, currentVersion, reportVersion)(implicitly, implicitly, implicitly).map {
        verify(mockCompilePayloadService, times(0))
          .collatePayloadsAndUpdateCache(any(), any(),
            ArgumentMatchers.eq(currentVersion), ArgumentMatchers.eq(reportVersion), any(), any(), any())(any(), any())
        result => result.header.status mustBe NOT_FOUND
      }
    }


    "return 204 No Content when valid data return from repository - event wind up" in {
      val reportVersion = "2"
      val currentVersion = "1"
      when(mockCompilePayloadService.addRecordVersionToUserAnswersJson(any(), any(), any()))
        .thenReturn(uaJsonEventWindUpWithRecordVersion)
      when(mockEventReportCacheRepository.getUserAnswers(
        any(), any(), any())(any())
      ).thenReturn(Future.successful(Some(uaJsonEventWindUp)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), eqTo(pstr), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))
      when(mockEventReportConnector.compileEventReportSummary(any(), any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, responseJson.toString)))

      when(mockCompilePayloadService.collatePayloadsAndUpdateCache(any(), any(),
        any(), any(), any(), any(), any())(any(), any()))
        .thenReturn(Future.successful(Json.obj()))

      eventReportService.compileEventReport(externalId, psaId, "pstr", WindUp, year, currentVersion, reportVersion).map {
        result =>
          verify(mockCompilePayloadService, times(1))
            .collatePayloadsAndUpdateCache(any(), any(),
              any(), any(), any(), any(), any())(any(), any())
          result.header.status mustBe NO_CONTENT
      }
    }

    "return 400 when validation errors response" in {
      when(mockCompilePayloadService.addRecordVersionToUserAnswersJson(any(), any(), any()))
        .thenReturn(uaJsonEventWindUpWithRecordVersion)
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), eqTo(pstr), eqTo(Some(EventDataIdentifier(WindUp, 2020, 1, externalId))))(any()))
        .thenReturn(Future.successful(Some(uaJsonEventWindUp)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), eqTo(pstr + "_original_cache"), eqTo(Some(EventDataIdentifier(WindUp, 2020, 1, externalId))))(any()))
        .thenReturn(Future.successful(Some(uaJsonEventWindUp)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), eqTo(pstr), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))

      when(mockJSONPayloadSchemaValidator.validatePayload(any(), eqTo(createCompiledEventSummaryReportSchemaPath), any()))
        .thenReturn(Failure(new Exception("Message")))

      recoverToExceptionIf[Exception] {
        eventReportService.compileEventReport(externalId, psaId, "pstr", WindUp, year, reportVersion, reportVersion)
      } map {
        failure =>
          failure.getMessage mustBe "Message"
      }
    }

    "throw Upstream5XXResponse on Internal Server Error" in {
      when(mockCompilePayloadService.addRecordVersionToUserAnswersJson(any(), any(), any()))
        .thenReturn(uaJsonEventWindUpWithRecordVersion)
      when(mockEventReportCacheRepository.getUserAnswers(any(), any(), any())(any()))
        .thenReturn(Future.successful(Some(uaJsonEventWindUp)))
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), eqTo(pstr), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(responseNoEventTypeJson)))

      when(mockEventReportConnector.compileEventReportSummary(any(), any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))

      recoverToExceptionIf[UpstreamErrorResponse] {
        eventReportService.compileEventReport(externalId, psaId, "pstr", WindUp, year, reportVersion, reportVersion)
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }
  }

  "memberChangeInfoTransformation" must {
    "deal with empty event properly" in {

      val data =
        """
          |{
          |		"event5" : {
          |			"members" : [
          |				{
          |					"membersDetails" : {
          |						"firstName" : "Member",
          |						"lastName" : "Three Changed",
          |						"nino" : "AA123434C"
          |					},
          |					"paymentDetails" : {
          |						"amountPaid" : 1,
          |						"eventDate" : "2017-01-01"
          |					}
          |				},
          |				{
          |					"membersDetails" : {
          |						"firstName" : "Member",
          |						"lastName" : "Four Changed",
          |						"nino" : "AA123435D"
          |					},
          |					"paymentDetails" : {
          |						"amountPaid" : 1,
          |						"eventDate" : "2017-01-01"
          |					}
          |				}
          |			]
          |		}
          |}
          |""".stripMargin

      val originalCache = """{}"""

      val result = eventReportService.memberChangeInfoTransformation(
        Some(Json.parse(originalCache).as[JsObject]),
        Json.parse(data).as[JsObject],
        Event5,
        "pstr",
        1,
        false
      )

      val expected = Json.parse("""{
                                  |  "event5" : {
                                  |    "members" : [ {
                                  |      "membersDetails" : {
                                  |        "firstName" : "Member",
                                  |        "lastName" : "Three Changed",
                                  |        "nino" : "AA123434C"
                                  |      },
                                  |      "paymentDetails" : {
                                  |        "amountPaid" : 1,
                                  |        "eventDate" : "2017-01-01"
                                  |      },
                                  |      "memberStatus" : "New"
                                  |    }, {
                                  |      "membersDetails" : {
                                  |        "firstName" : "Member",
                                  |        "lastName" : "Four Changed",
                                  |        "nino" : "AA123435D"
                                  |      },
                                  |      "paymentDetails" : {
                                  |        "amountPaid" : 1,
                                  |        "eventDate" : "2017-01-01"
                                  |      },
                                  |      "memberStatus" : "New"
                                  |    } ]
                                  |  }
                                  |}""".stripMargin)

      result mustBe expected
    }

    "changed members must not include amendedVersion, unchanged members must include amendedVersion" in {

      val data =
        """
          |{
          |		"event6" : {
          |			"members" : [
          |				{
          |					"inputProtectionType" : "Sample text",
          |					"membersDetails" : {
          |						"firstName" : "SarahA",
          |						"lastName" : "Urqhart",
          |						"nino" : "AA345678B"
          |					},
          |					"typeOfProtection" : "enhancedLifetimeAllowance",
          |					"memberStatus" : "New",
          |					"AmountCrystallisedAndDate" : {
          |						"amountCrystallised" : 600.67,
          |						"crystallisedDate" : "2021-05-30"
          |					},
          |					"amendedVersion" : "001"
          |				},
          |				{
          |					"inputProtectionType" : "Sample text",
          |					"membersDetails" : {
          |						"lastName" : "UrqhartChanged",
          |						"firstName" : "Sarah",
          |						"nino" : "AA345678C"
          |					},
          |					"typeOfProtection" : "enhancedLifetimeAllowance",
          |					"memberStatus" : "Deleted",
          |					"AmountCrystallisedAndDate" : {
          |						"amountCrystallised" : 600.67,
          |						"crystallisedDate" : "2021-05-30"
          |					},
          |					"amendedVersion" : "001"
          |				},
          |				{
          |					"inputProtectionType" : "Sample text",
          |					"membersDetails" : {
          |						"lastName" : "UrqhartDeleted",
          |						"firstName" : "Sarah",
          |						"nino" : "AA345678C"
          |					},
          |					"typeOfProtection" : "enhancedLifetimeAllowance",
          |					"memberStatus" : "Deleted",
          |					"AmountCrystallisedAndDate" : {
          |						"amountCrystallised" : 600.67,
          |						"crystallisedDate" : "2021-05-30"
          |					},
          |					"amendedVersion" : "001"
          |				}
          |			]
          |		}
          |	}
          |""".stripMargin

      val originalCache =
        """
          |{
          |		"event6" : {
          |			"members" : [
          |				{
          |					"inputProtectionType" : "Sample text",
          |					"membersDetails" : {
          |						"lastName" : "Urqhart",
          |						"firstName" : "Sarah",
          |						"nino" : "AA345678C"
          |					},
          |					"typeOfProtection" : "enhancedLifetimeAllowance",
          |					"memberStatus" : "New",
          |					"AmountCrystallisedAndDate" : {
          |						"amountCrystallised" : 600.67,
          |						"crystallisedDate" : "2021-05-30"
          |					},
          |					"amendedVersion" : "001"
          |				},
          |				{
          |					"inputProtectionType" : "Sample text",
          |					"membersDetails" : {
          |						"lastName" : "UrqhartChanged",
          |						"firstName" : "Sarah",
          |						"nino" : "AA345678C"
          |					},
          |					"typeOfProtection" : "enhancedLifetimeAllowance",
          |					"memberStatus" : "Changed",
          |					"AmountCrystallisedAndDate" : {
          |						"amountCrystallised" : 600.67,
          |						"crystallisedDate" : "2021-05-30"
          |					},
          |					"amendedVersion" : "001"
          |				},
          |				{
          |					"inputProtectionType" : "Sample text",
          |					"membersDetails" : {
          |						"lastName" : "UrqhartDeleted",
          |						"firstName" : "Sarah",
          |						"nino" : "AA345678C"
          |					},
          |					"typeOfProtection" : "enhancedLifetimeAllowance",
          |					"memberStatus" : "Deleted",
          |					"AmountCrystallisedAndDate" : {
          |						"amountCrystallised" : 600.67,
          |						"crystallisedDate" : "2021-05-30"
          |					},
          |					"amendedVersion" : "001"
          |				}
          |			]
          |		}
          |	}
          |""".stripMargin

      val result = eventReportService.memberChangeInfoTransformation(
        Some(Json.parse(originalCache).as[JsObject]),
        Json.parse(data).as[JsObject],
        Event6,
        "pstr",
        2,
        false
      )

      val expected = Json.parse(
        """{
          |  "event6" : {
          |    "members" : [ {
          |      "inputProtectionType" : "Sample text",
          |      "membersDetails" : {
          |        "firstName" : "SarahA",
          |        "lastName" : "Urqhart",
          |        "nino" : "AA345678B"
          |      },
          |      "typeOfProtection" : "enhancedLifetimeAllowance",
          |      "memberStatus" : "Changed",
          |      "AmountCrystallisedAndDate" : {
          |        "amountCrystallised" : 600.67,
          |        "crystallisedDate" : "2021-05-30"
          |      }
          |    }, {
          |      "inputProtectionType" : "Sample text",
          |      "membersDetails" : {
          |        "lastName" : "UrqhartChanged",
          |        "firstName" : "Sarah",
          |        "nino" : "AA345678C"
          |      },
          |      "typeOfProtection" : "enhancedLifetimeAllowance",
          |      "memberStatus" : "Deleted",
          |      "AmountCrystallisedAndDate" : {
          |        "amountCrystallised" : 600.67,
          |        "crystallisedDate" : "2021-05-30"
          |      }
          |    }, {
          |      "inputProtectionType" : "Sample text",
          |      "membersDetails" : {
          |        "lastName" : "UrqhartDeleted",
          |        "firstName" : "Sarah",
          |        "nino" : "AA345678C"
          |      },
          |      "typeOfProtection" : "enhancedLifetimeAllowance",
          |      "memberStatus" : "Deleted",
          |      "AmountCrystallisedAndDate" : {
          |        "amountCrystallised" : 600.67,
          |        "crystallisedDate" : "2021-05-30"
          |      },
          |      "amendedVersion" : "001"
          |    } ]
          |  }
          |}""".stripMargin)

      result mustBe expected
    }
  }

  "getEvent" must {
    "return OK if no data was found in the cache to begin with" in {
      when(mockEventReportConnector.getEvent(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(reportVersion),
        ArgumentMatchers.eq(Some(Event22)))(any(), any()))
        .thenReturn(Future.successful(getEvent22PayLoadData))

      eventReportService.getEvent(pstr, startDate, reportVersion.toInt, Event22)(implicitly, implicitly).map { resultJsValue =>
        verify(mockEventReportConnector, times(1)).getEvent(any(), any(), any(), any())(any(), any())
        resultJsValue mustBe Some(Json.toJson(getEvent22UAData))
      }
    }
  }

  "getEventSummary" must {
    "return the payload from the connector for API1834 and API1831" in {
      when(mockEventReportConnector.getEvent(pstr, startDate, reportVersion, None)(implicitly, implicitly))
        .thenReturn(Future.successful(responseJsonForAPI1834))
      when(mockEventReportConnector.getEvent(pstr, startDate, reportVersion, Some(Event20A))(implicitly, implicitly))
        .thenReturn(Future.successful(responseJsonForAPI1831))
      eventReportService.getEventSummary(pstr, reportVersion, startDate, testValue, testValue, Some(testName)).map { result =>
        verify(mockEventReportConnector, times(1)).getEvent(pstr, startDate, reportVersion, None)(implicitly, implicitly)
        verify(mockEventReportConnector, times(1)).getEvent(pstr, startDate, reportVersion, Some(Event20A))(implicitly, implicitly)

        val expected = Json.parse(
          """ [{"eventType":"1","recordVersion":2},
            |{"eventType":"2","recordVersion":1},
            |{"eventType":"3","recordVersion":2},
            |{"eventType":"4","recordVersion":1},
            |{"eventType":"5","recordVersion":4},
            |{"eventType":"6","recordVersion":7},
            |{"eventType":"7","recordVersion":2},
            |{"eventType":"8","recordVersion":4},
            |{"eventType":"8A","recordVersion":3},
            |{"eventType":"22","recordVersion":4},
            |{"eventType":"23","recordVersion":3},
            |{"eventType":"10","recordVersion":1},
            |{"eventType":"11","recordVersion":1},
            |{"eventType":"12","recordVersion":1},
            |{"eventType":"13","recordVersion":1},
            |{"eventType":"14","recordVersion":1},
            |{"eventType":"18","recordVersion":1},
            |{"eventType":"19","recordVersion":1},
            |{"eventType":"20","recordVersion":1},
            |{"eventType":"WindUp","recordVersion":1},
            |{"eventType":"20A","recordVersion":1}]""".stripMargin).as[JsArray]
        result mustBe expected
      }
    }

    "return locked event with locked parameter that includes name of the user who locked the event" in {
      when(mockEventReportConnector.getEvent(pstr, startDate, reportVersion, None)(implicitly, implicitly))
        .thenReturn(Future.successful(responseJsonForAPI1834))
      when(mockEventReportConnector.getEvent(pstr, startDate, reportVersion, Some(Event20A))(implicitly, implicitly))
        .thenReturn(Future.successful(responseJsonForAPI1831))
      when(mockEventLockRepository.getLockedEventTypes(any(), any(), any(), any(), any()))
        .thenReturn(Future.successful(Seq(EventType.Event8)))
      eventReportService.getEventSummary(pstr, reportVersion, startDate, testValue, testValue, Some(testName)).map { result =>
        verify(mockEventReportConnector, times(1)).getEvent(pstr, startDate, reportVersion, None)(implicitly, implicitly)
        verify(mockEventReportConnector, times(1)).getEvent(pstr, startDate, reportVersion, Some(Event20A))(implicitly, implicitly)

        val expected = Json.parse(
          s""" [{"eventType":"1","recordVersion":2},
            |{"eventType":"2","recordVersion":1},
            |{"eventType":"3","recordVersion":2},
            |{"eventType":"4","recordVersion":1},
            |{"eventType":"5","recordVersion":4},
            |{"eventType":"6","recordVersion":7},
            |{"eventType":"7","recordVersion":2},
            |{"eventType":"8","recordVersion":4,"lockedBy":"${testName.name.get} ${testName.lastName.get}"},
            |{"eventType":"8A","recordVersion":3},
            |{"eventType":"22","recordVersion":4},
            |{"eventType":"23","recordVersion":3},
            |{"eventType":"10","recordVersion":1},
            |{"eventType":"11","recordVersion":1},
            |{"eventType":"12","recordVersion":1},
            |{"eventType":"13","recordVersion":1},
            |{"eventType":"14","recordVersion":1},
            |{"eventType":"18","recordVersion":1},
            |{"eventType":"19","recordVersion":1},
            |{"eventType":"20","recordVersion":1},
            |{"eventType":"WindUp","recordVersion":1},
            |{"eventType":"20A","recordVersion":1}]""".stripMargin).as[JsArray]
        result mustBe expected
      }
    }
  }

  "saveEventToMongo" must {
    "return the payload from the connector when valid event type" in {
      when(mockEventReportCacheRepository.upsert(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(EventDataIdentifier(Event3, year, reportVersion.toInt, externalId)),
        any()
      )(any()))
        .thenReturn(Future.successful((): Unit))

      eventReportService.saveUserAnswers(externalId, pstr, EventType.Event3, year, reportVersion.toInt, payload, testValue)(implicitly).map { result =>
        verify(mockEventReportCacheRepository, times(1)).upsert(
          ArgumentMatchers.eq(pstr),
          ArgumentMatchers.eq(EventDataIdentifier(Event3, year, reportVersion.toInt, externalId)), any())(any())
        result mustBe(true)
      }
    }

    "return false when event is locked" in {
      when(mockEventReportCacheRepository.upsert(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(EventDataIdentifier(Event3, year, reportVersion.toInt, externalId)),
        any()
      )(any()))
        .thenReturn(Future.successful((): Unit))
      when(mockEventLockRepository.upsertIfNotLocked(any(),any(), any())).thenReturn(Future.successful(false))


      eventReportService.saveUserAnswers(externalId, pstr, EventType.Event3, year, reportVersion.toInt, payload, testValue)(implicitly).map { result =>
        verify(mockEventLockRepository, times(1)).upsertIfNotLocked(
          any(),
          any(),
          any()
        )
        result mustBe (false)
      }
    }
  }

  "removeUserAnswers" must {
    "return unit when data deleted" in {
      when(mockEventReportCacheRepository.removeAllOnSignOut(
        ArgumentMatchers.eq(externalId)
      )(any()))
        .thenReturn(Future.successful((): Unit))
      eventReportService.removeUserAnswers(externalId)(implicitly).map { result =>
        verify(mockEventReportCacheRepository, times(1)).removeAllOnSignOut(ArgumentMatchers.eq(externalId))(any())
        result mustBe()
      }
    }
  }

  "getUserAnswers with event type" must {
    "return the payload from the connector when valid event type" in {
      val json = Json.obj("test" -> "test")
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), eqTo(pstr), eqTo(Some(EventDataIdentifier(Event3, 2020, 1, externalId))))(any()))
        .thenReturn(Future.successful(Some(json)))

      eventReportService.getUserAnswers(externalId, pstr, EventType.Event3, year, reportVersion.toInt, testValue)(implicitly, implicitly).map { result =>
        result mustBe Some(json)
      }
    }

    "contain locked boolean in response if event is locked" in {
      val json = Json.obj("test" -> "test", "locked" -> true)
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), eqTo(pstr), eqTo(Some(EventDataIdentifier(Event3, 2020, 1, externalId))))(any()))
        .thenReturn(Future.successful(Some(json)))
      when(mockEventLockRepository.eventIsLocked(any(), any(), any())).thenReturn(Future.successful(true))
      eventReportService.getUserAnswers(externalId, pstr, EventType.Event3, year, reportVersion.toInt, testValue)(implicitly, implicitly).map { result =>
        result mustBe Some(json)
      }
    }


  }

  "getUserAnswers with NO event type" must {
    "return the payload from the connector when valid event type" in {
      val json = Json.obj("test" -> "test")
      when(mockEventReportCacheRepository.getUserAnswers(eqTo(externalId), eqTo(pstr), eqTo(None))(any()))
        .thenReturn(Future.successful(Some(json)))

      eventReportService.getUserAnswers(externalId, pstr)(implicitly).map { result =>
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

      eventReportService.getVersions(pstr, startDate)(implicitly, implicitly).map { result =>
        result mustBe erVersionsAfterTransformation
      }
    }
  }

  "getOverview" must {
    "return OK with the Seq of overview details" in {
      val overview1 = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = false,
        Some(EROverviewVersion(
          3,
          submittedVersionAvailable = false,
          compiledVersionAvailable = true)))

      val overview2 = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = false,
        Some(EROverviewVersion(
          2,
          submittedVersionAvailable = true,
          compiledVersionAvailable = true)))

      val erOverview = Seq(overview1)
      val er20AOverview = Seq(overview2)

      val expected = Seq(
        EROverview(
          LocalDate.of(2022, 4, 6),
          LocalDate.of(2023, 4, 5),
          tpssReportPresent = false,
          Some(EROverviewVersion(
            3,
            submittedVersionAvailable = true,
            compiledVersionAvailable = true)))
      )

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

        resultJsValue mustBe Json.toJson(expected)
      }
    }
  }

  "submitEventDeclarationReport" must {

    "return the result BadRequest when declaration has already done with same psaId and pstr" in {
      val (userAnswers, submitEventDeclarationReportSuccessResponseETMP) = super[GeneratorAPI1828].generateUserAnswersAndPOSTBody.sample.value
      when(mockDeclarationLockRepository.insertDoubleClickLock(any(), any())).
        thenReturn(Future.successful(false))
      when(mockEventReportConnector.submitEventDeclarationReport(
        ArgumentMatchers.eq(pstr), any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))

      eventReportService.submitEventDeclarationReport(pstr, psaId, userAnswers, reportVersion)(implicitly, implicitly, implicitly).map {
        response =>
          verify(mockEventReportConnector, times(0)).submitEventDeclarationReport(ArgumentMatchers.eq(pstr),
            ArgumentMatchers.eq(submitEventDeclarationReportSuccessResponseETMP), ArgumentMatchers.eq(reportVersion))(any(), any(), any())
          verify(mockDeclarationLockRepository, times(1)).insertDoubleClickLock(any(), any())
          response mustBe BadRequest
      }
    }

    "return valid response where there are changes" in {
      val (userAnswers, submitEventDeclarationReportSuccessResponseETMP) = super[GeneratorAPI1828].generateUserAnswersAndPOSTBody.sample.value
      when(mockDeclarationLockRepository.insertDoubleClickLock(any(), any())).
        thenReturn(Future.successful(true))
      when(mockEventReportConnector.submitEventDeclarationReport(
        ArgumentMatchers.eq(pstr), any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))
      eventReportService.submitEventDeclarationReport(pstr, psaId, userAnswers, reportVersion)(implicitly, implicitly, implicitly).map { _ =>
        verify(mockEventReportConnector, times(1)).submitEventDeclarationReport(ArgumentMatchers.eq(pstr),
          ArgumentMatchers.eq(submitEventDeclarationReportSuccessResponseETMP), ArgumentMatchers.eq(reportVersion))(any(), any(), any())
        verify(mockDeclarationLockRepository, times(1)).insertDoubleClickLock(any(), any())
        assert(true)
      }
    }
    "return valid an invalid response where an 1829 payload is passed" in {
      val userAnswers = super[GeneratorAPI1829].generateUserAnswersAndPOSTBody.sample.value._1
      when(mockDeclarationLockRepository.insertDoubleClickLock(any(), any())).
        thenReturn(Future.successful(true))
      when(mockEventReportConnector.submitEventDeclarationReport(
        ArgumentMatchers.eq(pstr), any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))
      eventReportService.submitEventDeclarationReport(pstr, psaId, userAnswers, reportVersion)(implicitly, implicitly, implicitly).map { _ =>
        verify(mockEventReportConnector, times(1)).submitEventDeclarationReport(ArgumentMatchers.eq(pstr),
          any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any())
        verify(mockDeclarationLockRepository, times(1)).insertDoubleClickLock(any(), any())
        assert(true)
      }
    }
    "return a 417 error response when there is nothing to submit" in {
      val (userAnswers, submitEventDeclarationReportSuccessResponseETMP) = super[GeneratorAPI1828].generateUserAnswersAndPOSTBody.sample.value
      when(mockDeclarationLockRepository.insertDoubleClickLock(any(), any())).
        thenReturn(Future.successful(true))
      when(mockEventReportConnector.submitEventDeclarationReport(
        ArgumentMatchers.eq(pstr), any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any()))
        .thenReturn(Future.failed(new BadRequestException("Test")))
      recoverToExceptionIf[ExpectationFailedException] {
        eventReportService.submitEventDeclarationReport(pstr, psaId, userAnswers, reportVersion)(implicitly, implicitly, implicitly)
      } map { _ =>
        verify(mockEventReportConnector, times(1)).submitEventDeclarationReport(ArgumentMatchers.eq(pstr),
          ArgumentMatchers.eq(submitEventDeclarationReportSuccessResponseETMP), ArgumentMatchers.eq(reportVersion))(any(), any(), any())
        verify(mockDeclarationLockRepository, times(1)).insertDoubleClickLock(any(), any())
        assert(true)
      }
    }
    "return a Future failed when validation fails against the schema for API1828" in {
      val (userAnswers, submitEventDeclarationReportSuccessResponseETMP) = super[GeneratorAPI1828].generateUserAnswersAndPOSTBody.sample.value
      when(mockDeclarationLockRepository.insertDoubleClickLock(any(), any())).
        thenReturn(Future.successful(true))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), eqTo(SchemaPath1828), any())).thenReturn(Failure(new Exception("Message")))
      when(mockEventReportConnector.submitEventDeclarationReport(
        ArgumentMatchers.eq(pstr), any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))

      recoverToExceptionIf[Exception] {
        eventReportService.submitEventDeclarationReport(pstr, psaId, userAnswers, reportVersion)(implicitly, implicitly, implicitly)
      } map { _ =>
        verify(mockEventReportConnector, times(0)).submitEventDeclarationReport(ArgumentMatchers.eq(pstr),
          any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any())
        verify(mockJSONPayloadSchemaValidator, times(1)).validatePayload(ArgumentMatchers.eq(submitEventDeclarationReportSuccessResponseETMP),
          ArgumentMatchers.eq(SchemaPath1828), ArgumentMatchers.eq("submitEventDeclarationReport"))
        verify(mockDeclarationLockRepository, times(1)).insertDoubleClickLock(any(), any())
        assert(true)
      }
    }
  }

  "isNewReportDifferentToPrevious" must {
    "return false when there is no change from existing event report" in {
      implicit val ec: ExecutionContextExecutor = ExecutionContext.global
      when(mockEventReportCacheRepository.getUserAnswers(any(), any(), any())(any()))
        .thenReturn(Future.successful(Some(payload)))
      eventReportService.isNewReportDifferentToPrevious(externalId, pstr, 2024, 1, "1")(ec).map { result =>
        result mustBe false
      }
    }

    "return true when there is no change from existing event report" in {
      implicit val ec: ExecutionContextExecutor = ExecutionContext.global
      when(mockEventReportCacheRepository.getUserAnswers(externalId, pstr + "_original_cache", Some(EventDataIdentifier(Event1, 2024, 1, externalId)))(ec))
        .thenReturn(Future.successful(Some(payload)))
      when(mockEventReportCacheRepository.getUserAnswers(externalId, pstr, Some(EventDataIdentifier(Event1, 2024, 1, externalId)))(ec))
        .thenReturn(Future.successful(Some(payload2)))
      when(mockEventReportCacheRepository.getUserAnswers(externalId, pstr + "_original_cache", Some(EventDataIdentifier(Event1, 2024, 1, externalId)))(ec))
        .thenReturn(Future.successful(Some(payload)))
      eventReportService.isNewReportDifferentToPrevious(externalId, pstr, 2024, 1, "1")(ec).map { result =>
        result mustBe true
      }
    }



  }

  "submitEvent20ADeclarationReport" must {
    "return the result BadRequest when declaration has already done with same psaId and pstr" in {
      val (userAnswers, submitEvent20ADeclarationReportSuccessResponseETMP) = super[GeneratorAPI1829].generateUserAnswersAndPOSTBody.sample.value
      when(mockDeclarationLockRepository.insertDoubleClickLock(any(), any())).
        thenReturn(Future.successful(false))
      when(mockEventReportConnector.submitEvent20ADeclarationReport(
        ArgumentMatchers.eq(pstr),
        any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))
      eventReportService.submitEvent20ADeclarationReport(pstr, psaId, userAnswers, reportVersion)(implicitly, implicitly, implicitly).map {
        response =>
          verify(mockEventReportConnector, times(0)).submitEvent20ADeclarationReport(ArgumentMatchers.eq(pstr),
            ArgumentMatchers.eq(submitEvent20ADeclarationReportSuccessResponseETMP), ArgumentMatchers.eq(reportVersion))(any(), any(), any())
          verify(mockDeclarationLockRepository, times(1)).insertDoubleClickLock(any(), any())
          response mustBe BadRequest
      }
    }

    "return valid response where there are changes" in {
      val (userAnswers, submitEvent20ADeclarationReportSuccessResponseETMP) = super[GeneratorAPI1829].generateUserAnswersAndPOSTBody.sample.value
      when(mockDeclarationLockRepository.insertDoubleClickLock(any(), any())).
        thenReturn(Future.successful(true))
      when(mockEventReportConnector.submitEvent20ADeclarationReport(
        ArgumentMatchers.eq(pstr),
        any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))
      eventReportService.submitEvent20ADeclarationReport(pstr, psaId, userAnswers, reportVersion)(implicitly, implicitly, implicitly).map { _ =>
        verify(mockEventReportConnector, times(1)).submitEvent20ADeclarationReport(ArgumentMatchers.eq(pstr),
          ArgumentMatchers.eq(submitEvent20ADeclarationReportSuccessResponseETMP), ArgumentMatchers.eq(reportVersion))(any(), any(), any())
        verify(mockDeclarationLockRepository, times(1)).insertDoubleClickLock(any(), any())
        assert(true)
      }
    }
    "return a 417 error response when there is nothing to submit" in {
      val (userAnswers, submitEvent20ADeclarationReportSuccessResponseETMP) = super[GeneratorAPI1829].generateUserAnswersAndPOSTBody.sample.value
      when(mockDeclarationLockRepository.insertDoubleClickLock(any(), any())).
        thenReturn(Future.successful(true))
      when(mockEventReportConnector.submitEvent20ADeclarationReport(
        ArgumentMatchers.eq(pstr),
        any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any()))
        .thenReturn(Future.failed(new BadRequestException("Test")))
      recoverToExceptionIf[ExpectationFailedException] {
        eventReportService.submitEvent20ADeclarationReport(pstr, psaId, userAnswers, reportVersion)(implicitly, implicitly, implicitly)
      } map { _ =>
        verify(mockEventReportConnector, times(1)).submitEvent20ADeclarationReport(ArgumentMatchers.eq(pstr),
          ArgumentMatchers.eq(submitEvent20ADeclarationReportSuccessResponseETMP), ArgumentMatchers.eq(reportVersion))(any(), any(), any())
        verify(mockDeclarationLockRepository, times(1)).insertDoubleClickLock(any(), any())
        assert(true)
      }
    }
    "return a Future failed when validation fails against the schema for API1829" in {
      val (userAnswers, submitEvent20ADeclarationReportSuccessResponseETMP) = super[GeneratorAPI1829].generateUserAnswersAndPOSTBody.sample.value
      when(mockDeclarationLockRepository.insertDoubleClickLock(any(), any())).
        thenReturn(Future.successful(true))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), eqTo(SchemaPath1829), any())).thenReturn(Failure(new Exception("Message")))
      when(mockEventReportConnector.submitEvent20ADeclarationReport(
        ArgumentMatchers.eq(pstr), any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))
      recoverToExceptionIf[Exception] {
        eventReportService.submitEvent20ADeclarationReport(pstr, psaId, userAnswers, reportVersion)(implicitly, implicitly, implicitly)
      } map { _ =>
        verify(mockEventReportConnector, times(0)).submitEvent20ADeclarationReport(ArgumentMatchers.eq(pstr),
          any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any())
        verify(mockJSONPayloadSchemaValidator, times(1)).validatePayload(ArgumentMatchers.eq(submitEvent20ADeclarationReportSuccessResponseETMP),
          ArgumentMatchers.eq(SchemaPath1829), ArgumentMatchers.eq("submitEvent20ADeclarationReport"))
        verify(mockDeclarationLockRepository, times(1)).insertDoubleClickLock(any(), any())
        assert(true)
      }
    }
    "return valid an invalid response where an 1828 payload is passed" in {
      val userAnswers = super[GeneratorAPI1828].generateUserAnswersAndPOSTBody.sample.value._1
      when(mockDeclarationLockRepository.insertDoubleClickLock(any(), any())).
        thenReturn(Future.successful(true))
      when(mockEventReportConnector.submitEvent20ADeclarationReport(
        ArgumentMatchers.eq(pstr), any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any()))
        .thenReturn(Future.successful(HttpResponse.apply(
          status = OK,
          json = payload,
          headers = Map.empty)))
      eventReportService.submitEvent20ADeclarationReport(pstr, psaId, userAnswers, reportVersion)(implicitly, implicitly, implicitly).map { _ =>
        verify(mockEventReportConnector, times(1)).submitEvent20ADeclarationReport(ArgumentMatchers.eq(pstr),
          any(), ArgumentMatchers.eq(reportVersion))(any(), any(), any())
        verify(mockDeclarationLockRepository, times(1)).insertDoubleClickLock(any(), any())
        assert(true)
      }
    }
  }
}

object EventReportServiceSpec {
  private val responseJson: JsObject = Json.obj("event" -> "mockEvent - test passed")
  private val responseJsonEvent1: JsObject = Json.obj("event1" -> Json.obj("membersOrEmployers" -> JsArray()))
  private val responseJsonEvent1WithRecordVersion: JsObject = Json.obj("event1" -> Json.obj("membersOrEmployers" -> JsArray(), "recordVersion" -> 1))
  private val responseNoEventTypeJson: JsObject = Json.obj("taxYear" -> "2022")

  private val uaJsonEventWindUp: JsObject = Json.obj("eventWindUp" -> Json.obj("schemeWindUpDate" -> "2020-06-01"))
  private val uaJsonEventWindUpWithRecordVersion: JsObject = Json.obj("eventWindUp" -> Json.obj("schemeWindUpDate" -> "2020-06-01", "recordVersion" -> 1))

  private val createCompiledEventSummaryReportSchemaPath = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.1.0.json"
  private val compileEventOneReportSchemaPath = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.4.json"

  private final val SchemaPath1828 = "/resources.schemas/api-1828-submit-event-declaration-report-request-schema-v1.0.4.json"
  private final val SchemaPath1829 = "/resources.schemas/api-1829-submit-event20a-declaration-report-request-schema-v1.0.0.json"

  private val endDate = "2023-04-05"

  private val erVersions = Json.arr(
    Json.obj(
      "reportFormBundleNumber" -> "123456789012",
      "reportVersion" -> 1,
      "reportStatus" -> "Compiled",
      "compilationOrSubmissionDate" -> "2022-04-01T09:30:47Z",
      "reportSubmitterDetails" -> Json.obj(
        "reportSubmittedBy" -> "PSP",
        "organisationOrPartnershipDetails" -> Json.obj(
          "organisationOrPartnershipName" -> "ABC Limited"
        )
      ),
      "psaDetails" -> Json.obj(
        "psaOrganisationOrPartnershipDetails" -> Json.obj(
          "organisationOrPartnershipName" -> "XYZ Limited"
        )
      )
    )
  )

  private val erVersionsAfterTransformation = Json.arr(
    Json.obj(
      "versionDetails" -> Json.obj(
        "version"-> 1,
        "status"-> "compiled"
      ),
      "submitterName" -> "ABC Limited",
      "submittedDate"-> "2022-04-01"
    )
  )



  private val getEvent22PayLoadData: Option[JsObject] = Some(Json.parse(
    """
      |    {
      |    "eventDetails": [
      |    {
      |    "memberDetail": {
      |        "memberStatus": "New",
      |        "amendedVersion": "001",
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
      |          "memberStatus": "New",
      |          "amendedVersion": "001",
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

  private val responseJsonForAPI1831: Option[JsObject] = Some(Json.parse(
    """
      |{
      |    "schemeDetails": {
      |        "pSTR": "87219363YN",
      |        "schemeName": "Abc Ltd"
      |    },
      |    "er20aDetails": {
      |       "reportStartDate": "2021-04-06",
      |       "reportEndDate": "2022-04-05",
      |       "reportVersionNumber": "001",
      |       "reportSubmittedDateAndTime": "2023-12-13T12:12:12Z"
      |     },
      |    "schemeMasterTrustDetails": {
      |       "startDate": "2021-06-08"
      |    },
      |    "erDeclarationDetails": {
      |       "submittedBy": "PSP",
      |       "submittedID": "20000001",
      |       "submittedName": "ABCDEFGHIJKLMNOPQRSTUV",
      |       "pspDeclaration": {
      |         "authorisedPSAID": "A4045157",
      |         "pspDeclaration1": "Selected",
      |         "pspDeclaration2": "Selected"
      |         }
      |     }
      | }
      |""".stripMargin).as[JsObject])
}


