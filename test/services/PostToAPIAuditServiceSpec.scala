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

import audit.{CompileEventAuditEvent, SubmitEventDeclarationAuditEvent}
import base.SpecBase
import models.enumeration.EventType
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{doNothing, reset, times, verify}
import org.scalatest.BeforeAndAfterEach
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.RequestHeader
import play.api.test.FakeRequest
import services.AuditServiceSpec.mock
import uk.gov.hmrc.http.{HttpException, HttpResponse, UpstreamErrorResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class PostToAPIAuditServiceSpec extends SpecBase with BeforeAndAfterEach {

  private implicit lazy val rh: RequestHeader = FakeRequest("", "")

  private val reportVersion = "1"
  private val mockAuditService = mock[AuditService]

  private val psaId = "psa"
  private val pstr = "pstr"
  private val requestData = Json.obj("test" -> "test")
  private val responseData = Json.obj("responsetest" -> "test")

  override def beforeEach(): Unit = {
    reset(mockAuditService)
  }

  "sendSubmitEventDeclarationAuditEvent" must {
    "send the correct audit event for a successful response" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendSubmitEventDeclarationAuditEvent(pstr, requestData, reportVersion, None)
      pf(Success(HttpResponse.apply(Status.OK, responseData, Map.empty)))
      val expectedAuditEvent = SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = Some(Status.OK),
        request = requestData,
        response = Some(responseData),
        maybeErrorMessage = None,
        reportVersion = reportVersion,
        maybeEventType = None
      )
      verify(mockAuditService, times(1)).sendEvent(ArgumentMatchers.eq(expectedAuditEvent))(any(), any())
    }

    "send the audit event with the status code when an upstream error occurs" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendSubmitEventDeclarationAuditEvent(pstr, requestData, reportVersion, Some(EventType.Event20A))
      val reportAs = 202
      val message = "The request was not found"
      val status = Status.NOT_FOUND
      pf(Failure(UpstreamErrorResponse.apply(message, status, reportAs, Map.empty)))
      val expectedAuditEvent = SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = Some(status),
        request = requestData,
        response = None,
        maybeErrorMessage = None,
        reportVersion = reportVersion,
        maybeEventType = Some(EventType.Event20A)
      )
      verify(mockAuditService, times(1)).sendEvent(ArgumentMatchers.eq(expectedAuditEvent))(any(), any())
    }

    "send the audit event with the status code when an HttpException error occurs" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendSubmitEventDeclarationAuditEvent(pstr, requestData, reportVersion, None)

      val message = "The request had a network error"
      val status = Status.SERVICE_UNAVAILABLE
      pf(Failure(new HttpException(message, status)))
      val expectedAuditEvent = SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = Some(status),
        request = requestData,
        response = None,
        maybeErrorMessage = None,
        reportVersion = reportVersion,
        maybeEventType = None
      )
      verify(mockAuditService, times(1)).sendEvent(ArgumentMatchers.eq(expectedAuditEvent))(any(), any())
    }

    "send the audit event when a throwable is thrown" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendSubmitEventDeclarationAuditEvent(pstr, requestData, reportVersion, None)

      val message = "The request had a network error"
      pf(Failure(new RuntimeException(message)))
      val expectedAuditEvent = SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = None,
        request = requestData,
        response = None,
        maybeErrorMessage = Some(message),
        reportVersion = reportVersion,
        maybeEventType = None
      )
      verify(mockAuditService, times(1)).sendEvent(ArgumentMatchers.eq(expectedAuditEvent))(any(), any())
    }
  }


  "sendCompileEventDeclarationAuditEvent" must {
    "send the correct audit event for a successful response" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendCompileEventDeclarationAuditEvent(psaId, pstr, requestData, reportVersion)
      pf(Success(HttpResponse.apply(Status.OK, responseData, Map.empty)))
      val expectedAuditEvent = CompileEventAuditEvent(
        psaPspIdentifier = psaId,
        pstr = pstr,
        payload = requestData,
        status = Some(Status.OK),
        response = Some(responseData),
        errorMessage = None,
        reportVersion
      )
      verify(mockAuditService, times(1)).sendEvent(ArgumentMatchers.eq(expectedAuditEvent))(any(), any())
    }

    "send the audit event with the status code when an upstream error occurs" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendCompileEventDeclarationAuditEvent(psaId, pstr, requestData, reportVersion)
      val reportAs = 202
      val message = "The request was not found"
      val status = Status.NOT_FOUND
      pf(Failure(UpstreamErrorResponse.apply(message, status, reportAs, Map.empty)))
      val expectedAuditEvent = CompileEventAuditEvent(
        psaPspIdentifier = psaId,
        pstr = pstr,
        payload = requestData,
        status = Some(status),
        response = None,
        errorMessage = None,
        reportVersion
      )
      verify(mockAuditService, times(1)).sendEvent(ArgumentMatchers.eq(expectedAuditEvent))(any(), any())
    }

    "send the audit event with the status code when an HttpException error occurs" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendCompileEventDeclarationAuditEvent(psaId, pstr, requestData, reportVersion)

      val message = "The request had a network error"
      val status = Status.SERVICE_UNAVAILABLE
      pf(Failure(new HttpException(message, status)))
      val expectedAuditEvent = CompileEventAuditEvent(
        psaPspIdentifier = psaId,
        pstr = pstr,
        payload = requestData,
        status = Some(status),
        response = None,
        errorMessage = None,
        reportVersion
      )
      verify(mockAuditService, times(1)).sendEvent(ArgumentMatchers.eq(expectedAuditEvent))(any(), any())
    }

    "send the audit event when a throwable is thrown" in {
      doNothing().when(mockAuditService).sendEvent(any())(any(), any())
      val service = new PostToAPIAuditService(mockAuditService)
      val pf = service.sendCompileEventDeclarationAuditEvent(psaId, pstr, requestData, reportVersion)

      val message = "The request had a network error"
      pf(Failure(new RuntimeException(message)))
      val expectedAuditEvent = CompileEventAuditEvent(
        psaPspIdentifier = psaId,
        pstr = pstr,
        payload = requestData,
        status = None,
        response = None,
        errorMessage = Some(message),
        reportVersion
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
        maybeErrorMessage = Some(errorMessage),
        reportVersion = reportVersion,
        maybeEventType = Some(EventType.Event20A)
      ).details mustBe Json.obj(
        "PensionSchemeTaxReference" -> "pstr",
        "status" -> 200,
        "request" -> requestData,
        "response" -> responseData,
        "errorMessage" -> errorMessage,
        "reportVersion" -> reportVersion,
        "eventNumber" -> EventType.Event20A.toString
      )
    }

    "render when all optional values are absent" in {
      SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = None,
        request = requestData,
        response = None,
        maybeErrorMessage = None,
        reportVersion = reportVersion,
        maybeEventType = None
      ).details mustBe Json.obj(
        "PensionSchemeTaxReference" -> "pstr",
        "request" -> requestData,
        "reportVersion" -> reportVersion
      )
    }
  }


  "CompileEventAuditEvent.details" must {

    "render when all optional values are present" in {
      val errorMessage = "error message"
      val event = CompileEventAuditEvent(
        psaPspIdentifier = psaId,
        pstr = pstr,
        payload = requestData,
        status = Some(Status.OK),
        response = Some(responseData),
        errorMessage = Some(errorMessage),
        reportVersion
      )

      val expected = Json.obj(
        "pspOrPsaId" -> psaId,
        "PensionSchemeTaxReference" -> "pstr",
        "status" -> 200,
        "payload" -> requestData,
        "response" -> responseData,
        "errorMessage" -> errorMessage,
        "reportVersion" -> reportVersion
      )

      event.details mustBe expected
      event.auditType mustBe "EventReportTaxReturnCompiled"
    }

    "render when all optional values are absent" in {
      val event = CompileEventAuditEvent(
        psaPspIdentifier = psaId,
        pstr = pstr,
        payload = requestData,
        status = None,
        response = None,
        errorMessage = None,
        reportVersion
      )

      val expected = Json.obj(
        "pspOrPsaId" -> psaId,
        "PensionSchemeTaxReference" -> "pstr",
        "payload" -> requestData,
        "reportVersion" -> reportVersion
      )

      event.details mustBe expected
      event.auditType mustBe "EventReportTaxReturnCompiled"
    }
  }
}