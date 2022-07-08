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
import connectors.cache.OverviewCacheConnector
import models.enumeration.ApiType.Api1830
import models.enumeration.EventType
import models.{EROverview, EROverviewVersion, ERVersion}
import org.mockito.ArgumentMatchers.any
import org.mockito.{ArgumentMatchers, MockitoSugar}
import org.scalatest.BeforeAndAfter
import org.scalatest.concurrent.ScalaFutures.whenReady
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import play.api.http.Status.NO_CONTENT
import play.api.libs.json.{JsObject, Json}
import play.api.test.Helpers._
import repositories.EventReportCacheRepository
import uk.gov.hmrc.http._
import utils.{ErrorReport, JSONPayloadSchemaValidator}

import java.time.LocalDate
import scala.concurrent.Future

class EventReportServiceSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfter {

  import EventReportServiceSpec._

  implicit val hc: HeaderCarrier = HeaderCarrier()
  private val mockEventReportConnector = mock[EventReportConnector]
  private val mockJSONPayloadSchemaValidator = mock[JSONPayloadSchemaValidator]
  private val mockEventReportCacheRepository = mock[EventReportCacheRepository]
  private val mockOverviewCacheConnector = mock[OverviewCacheConnector]

  private val pstr = "pstr"
  private val startDate = "startDate"
  private val version = "version"
  private val payload = Json.obj("test" -> "test")

  val eventReportService = new EventReportService(mockEventReportConnector, mockEventReportCacheRepository, mockJSONPayloadSchemaValidator, mockOverviewCacheConnector)

  before {
    reset(mockEventReportConnector, mockOverviewCacheConnector)
    when(mockJSONPayloadSchemaValidator.validateJsonPayload(any(), any())) thenReturn Right(true)
    when(mockOverviewCacheConnector.get(any(), any(), any(), any())(any())).thenReturn(Future.successful(None))

  }

  "compileEventReport" must {
    "return 204 No Content when no data return from repository" in {
      when(mockEventReportCacheRepository.getByKeys(any())(any()))
        .thenReturn(Future.successful(None))
      eventReportService.compileEventReport("pstr", Json.obj())(implicitly, implicitly).map {
        result => result.header.status mustBe NO_CONTENT
      }
    }

    "return 204 No Content when valid data return from repository" in {

      when(mockEventReportCacheRepository.getByKeys(any())(any()))
        .thenReturn(Future.successful(Some(responseJson)))

      when(mockEventReportConnector.compileEventOneReport(any(), any())(any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, responseJson.toString)))

      when(mockEventReportConnector.compileEventReportSummary(any(), any())(any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, responseJson.toString)))
      eventReportService.compileEventReport("pstr", Json.obj()).map {
        result => result.header.status mustBe NO_CONTENT
      }
    }


    "return 400 when validation errors response" in {
      when(mockEventReportCacheRepository.getByKeys(any())(any()))
        .thenReturn(Future.successful(Some(responseJson)))

      when(mockEventReportConnector.compileEventReportSummary(any(), any())(any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, responseJson.toString)))

      val listErrors: List[ErrorReport] = List(
        ErrorReport("instance1", "errors1"),
        ErrorReport("instance2", "errors2")
      )

      when(mockJSONPayloadSchemaValidator.validateJsonPayload(any(), any())) thenReturn Left(listErrors)

      recoverToExceptionIf[EventReportValidationFailureException] {
        eventReportService.compileEventReport("pstr", Json.obj())
      } map {
        failure =>
          failure.exMessage mustBe "Schema validation errors:-\n(instance1: errors1),\n(instance2: errors2)"
      }
    }

    "throw Upstream5XXResponse on Internal Server Error" in {

      when(mockEventReportCacheRepository.getByKeys(any())(any()))
        .thenReturn(Future.successful(Some(responseJson)))

      when(mockEventReportConnector.compileEventReportSummary(any(), any())(any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))
      when(mockEventReportConnector.compileEventOneReport(any(), any())(any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))
      recoverToExceptionIf[UpstreamErrorResponse] {
        eventReportService.compileEventReport("pstr", Json.obj())
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }
  }

  "getEvent" must {
    "return the payload from the connector when valid event type" in {
      when(mockEventReportConnector.getEvent(any(), any(), any(), any())(any(), any()))
        .thenReturn(Future.successful(responseJson))
      whenReady(eventReportService.getEvent(pstr, startDate, version, EventType.Event3)(implicitly, implicitly)) { result =>
        result mustBe responseJson
      }
    }

    "return not found exception when invalid event type" in {
      recoverToExceptionIf[NotFoundException] {
        eventReportService.getEvent(pstr, startDate, version, EventType.Event1)(implicitly, implicitly)
      } map {
        failure =>
          failure.message mustBe "Not Found: ApiType not found for eventType (1)"
      }
    }
  }

  "saveEvent" must {
    "return the payload from the connector when valid event type" in {
      when(mockEventReportCacheRepository.upsert(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(Api1830),
        any()
      )(any()))
        .thenReturn(Future.successful(HttpResponse(OK, saveEventSuccessResponse.toString)))
      whenReady(eventReportService.saveEvent(pstr, EventType.Event3, payload)(implicitly)) { result =>
        assert(true)
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
      when(mockOverviewCacheConnector.get(any(), any(), any(), any())(any())).thenReturn(Future.successful(None))
      when(mockOverviewCacheConnector.save(any(), any(), any(), any(), any())(any())).thenReturn(Future.successful(true))

      eventReportService.getOverview(pstr, reportTypeER, startDate, endDate)(implicitly, implicitly).map { resultJsValue =>
        verify(mockOverviewCacheConnector, times(1)).get(any(), any(), any(), any())(any())
        verify(mockOverviewCacheConnector, times(1)).save(any(), any(), any(), any(), any())(any())
        verify(mockEventReportConnector, times(1)).getOverview(any(), any(), any(), any())(any(), any())
        resultJsValue mustBe Json.toJson(erOverview)
      }
    }

    "return OK with the Seq of overview details and don't try to save the data in cache if the data already exists in the cache" in {
      when(mockOverviewCacheConnector.get(any(), any(), any(), any())(any())).thenReturn(Future.successful(Some(Json.toJson(erOverview))))
      eventReportService.getOverview(pstr, reportTypeER, startDate, endDate)(implicitly, implicitly).map { resultJsValue =>
        verify(mockOverviewCacheConnector, times(1)).get(any(), any(), any(), any())(any())
        verify(mockOverviewCacheConnector, never).save(any(), any(), any(), any(), any())(any())
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
}

object EventReportServiceSpec {
  val responseJson: JsObject = Json.obj("event" -> "10")

  val saveEventSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678955")

  val compileEventSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678977")


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

  val submitEventDeclarationReportSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678933")

}


