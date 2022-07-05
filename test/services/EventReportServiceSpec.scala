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
import org.mockito.ArgumentMatchers.any
import org.mockito.MockitoSugar
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import play.api.http.Status.NO_CONTENT
import play.api.libs.json.{JsObject, Json}
import play.api.test.Helpers._
import repositories.EventReportCacheRepository
import uk.gov.hmrc.http._
import utils.{ErrorReport, JSONPayloadSchemaValidator}

import scala.concurrent.Future

class EventReportServiceSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfter {

  import EventReportServiceSpec._

  implicit val hc: HeaderCarrier = HeaderCarrier()
  private val mockEventReportConnector = mock[EventReportConnector]
  private val mockJSONPayloadSchemaValidator = mock[JSONPayloadSchemaValidator]
  private val mockEventReportCacheRepository = mock[EventReportCacheRepository]
  val eventReportService = new EventReportService(mockEventReportConnector, mockEventReportCacheRepository, mockJSONPayloadSchemaValidator)

  before {
    reset(mockEventReportConnector)
    when(mockJSONPayloadSchemaValidator.validateJsonPayload(any(), any())) thenReturn Right(true)

  }

  "compileEventReport" must {
    "return 204 No Content when no data return form repository" in {
      when(mockEventReportCacheRepository.getByKeys(any())(any()))
        .thenReturn(Future.successful(None))
      eventReportService.compileEventReport("pstr", Json.obj())(implicitly, implicitly).map {
        result => result.header.status mustBe NO_CONTENT
      }
    }

    "return 204 No Content when valid data return form repository" in {

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
      recoverToExceptionIf[UpstreamErrorResponse] {
        eventReportService.compileEventReport("pstr", Json.obj())
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }
  }
}

object EventReportServiceSpec {
  val responseJson: JsObject = Json.obj("event" -> "10")
}


