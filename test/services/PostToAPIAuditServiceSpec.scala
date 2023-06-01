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

import audit.SubmitEventDeclarationAuditEvent
import base.SpecBase
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{doNothing, reset, times, verify}
import org.scalatest.BeforeAndAfterEach
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.RequestHeader
import play.api.test.FakeRequest
import services.AuditServiceSpec.mock
import uk.gov.hmrc.http.{HttpException, HttpResponse, Upstream4xxResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
class PostToAPIAuditServiceSpec extends SpecBase with BeforeAndAfterEach {

  private implicit lazy val rh: RequestHeader = FakeRequest("", "")

  private val mockAuditService = mock[AuditService]

  private val pstr = "pstr"
  private val requestData = Json.obj("test" -> "test")
  private val responseData = Json.obj("responsetest" -> "test")

  override def beforeEach(): Unit = {
    reset(mockAuditService)
  }

  "SubmitEventDeclarationAuditService" must {
    "send the correct audit event for a successful response" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendSubmitEventDeclarationAuditEvent(pstr, requestData)
      pf(Success(HttpResponse.apply(Status.OK, responseData, Map.empty)))
      val expectedAuditEvent = SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = Some(Status.OK),
        request = requestData,
        response = Some(responseData),
        maybeErrorMessage = None
      )
      verify(mockAuditService, times(1)).sendEvent(ArgumentMatchers.eq(expectedAuditEvent))(any(), any())
    }

    "send the audit event with the status code when an upstream error occurs" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendSubmitEventDeclarationAuditEvent(pstr, requestData)
      val reportAs = 202
      val message = "The request was not found"
      val status = Status.NOT_FOUND
      pf(Failure(Upstream4xxResponse.apply(message, status, reportAs, Map.empty)))
      val expectedAuditEvent = SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = Some(status),
        request = requestData,
        response = None,
        maybeErrorMessage = None
      )
      verify(mockAuditService, times(1)).sendEvent(ArgumentMatchers.eq(expectedAuditEvent))(any(), any())
    }

    "send the audit event with the status code when an HttpException error occurs" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendSubmitEventDeclarationAuditEvent(pstr, requestData)

      val message = "The request had a network error"
      val status = Status.SERVICE_UNAVAILABLE
      pf(Failure(new HttpException(message, status)))
      val expectedAuditEvent = SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = Some(status),
        request = requestData,
        response = None,
        maybeErrorMessage = None
      )
      verify(mockAuditService, times(1)).sendEvent(ArgumentMatchers.eq(expectedAuditEvent))(any(), any())
    }

    "send the audit event when a throwable is thrown" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendSubmitEventDeclarationAuditEvent(pstr, requestData)

      val message = "The request had a network error"
      val status = Status.SERVICE_UNAVAILABLE
      pf(Failure(new RuntimeException(message)))
      val expectedAuditEvent = SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = None,
        request = requestData,
        response = None,
        maybeErrorMessage = Some(message)
      )
      verify(mockAuditService, times(1)).sendEvent(ArgumentMatchers.eq(expectedAuditEvent))(any(), any())
    }
  }

  "SubmitEventDeclarationAuditService.details" must {
    "render when all optional values are present" in {
      val errorMessage = "error message"
      SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = Some(Status.OK),
        request = requestData,
        response = Some(responseData),
        maybeErrorMessage = Some(errorMessage)
      ).details mustBe Json.obj(
        "pstr" -> "pstr",
        "status" -> 200,
        "request" -> requestData,
        "response" -> responseData,
        "errorMessage" -> errorMessage
      )
    }

    "render when all optional values are absent" in {
      val errorMessage = "error message"
      SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = None,
        request = requestData,
        response = None,
        maybeErrorMessage = None
      ).details mustBe Json.obj(
        "pstr" -> "pstr",
        "request" -> requestData
      )
    }
  }
}