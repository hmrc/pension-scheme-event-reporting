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
import com.google.inject.Inject
import models.enumeration.EventType
import play.api.http.Status
import play.api.libs.json._
import play.api.mvc.RequestHeader
import uk.gov.hmrc.http.{HttpException, HttpResponse, UpstreamErrorResponse}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

class PostToAPIAuditService @Inject()(auditService: AuditService) {
  def sendSubmitEventDeclarationAuditEvent(pstr: String, data: JsValue, reportVersion: String, maybeEventType: Option[EventType])
                                          (implicit ec: ExecutionContext, request: RequestHeader): PartialFunction[Try[HttpResponse], Unit] = {
    case Success(httpResponse) =>
      auditService.sendEvent(SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = Some(Status.OK),
        request = data,
        reportVersion = reportVersion,
        response = Some(httpResponse.json),
        maybeErrorMessage = None,
        maybeEventType = maybeEventType
      ))
    case Failure(error: UpstreamErrorResponse) =>
      auditService.sendEvent(
        SubmitEventDeclarationAuditEvent(
          pstr,
          maybeStatus = Some(error.statusCode),
          request = data,
          reportVersion = reportVersion,
          response = None,
          maybeErrorMessage = None,
          maybeEventType = maybeEventType
        ))
    case Failure(error: HttpException) =>
      auditService.sendEvent(SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = Some(error.responseCode),
        request = data,
        reportVersion = reportVersion,
        response = None,
        maybeErrorMessage = None,
        maybeEventType = maybeEventType
      ))

    case Failure(error: Throwable) =>
      auditService.sendEvent(SubmitEventDeclarationAuditEvent(
        pstr = pstr,
        maybeStatus = None,
        request = data,
        reportVersion = reportVersion,
        response = None,
        maybeErrorMessage = Some(error.getMessage),
        maybeEventType = maybeEventType
      ))
  }

  def sendCompileEventDeclarationAuditEvent(psaPspIdentifier: String, pstr: String, payload: JsValue, reportVersion: String)
                                           (implicit ec: ExecutionContext, request: RequestHeader): PartialFunction[Try[HttpResponse], Unit] = {
    case Success(httpResponse) =>
      auditService.sendEvent(CompileEventAuditEvent(
        psaPspIdentifier = psaPspIdentifier,
        pstr = pstr,
        payload = payload,
        reportVersion = reportVersion,
        status = Some(httpResponse.status),
        response = Some(httpResponse.json),
        errorMessage = None
      ))
    case Failure(error: UpstreamErrorResponse) =>
      auditService.sendEvent(CompileEventAuditEvent(
        psaPspIdentifier = psaPspIdentifier,
        pstr = pstr,
        payload = payload,
        reportVersion = reportVersion,
        status = Some(error.statusCode),
        response = None,
        errorMessage = None
      ))
    case Failure(error: HttpException) =>
      auditService.sendEvent(CompileEventAuditEvent(
        psaPspIdentifier = psaPspIdentifier,
        pstr = pstr,
        payload = payload,
        reportVersion = reportVersion,
        status = Some(error.responseCode),
        response = None,
        errorMessage = None
      ))

    case Failure(error: Throwable) =>
      auditService.sendEvent(CompileEventAuditEvent(
        psaPspIdentifier = psaPspIdentifier,
        pstr = pstr,
        payload = payload,
        reportVersion = reportVersion,
        status = None,
        response = None,
        errorMessage = Some(error.getMessage)
      ))
  }
}
