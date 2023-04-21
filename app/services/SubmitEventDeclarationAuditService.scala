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

import audit.AuditEvent
import com.google.inject.Inject
import play.api.http.Status
import play.api.libs.json._
import play.api.mvc.RequestHeader
import uk.gov.hmrc.http.{HttpException, HttpResponse, UpstreamErrorResponse}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

class SubmitEventDeclarationAuditService @Inject()(auditService: AuditService) {
  def sendSubmitEventDeclarationAuditEvent(pstr: String, data: JsValue)
                                          (implicit ec: ExecutionContext, request: RequestHeader): PartialFunction[Try[HttpResponse], Unit] = {
    case Success(httpResponse) =>
      auditService.sendEvent(SubmitEventDeclarationAuditEvent(pstr, Status.OK, data, Some(httpResponse.json)))
    case Failure(error: UpstreamErrorResponse) =>
      auditService.sendEvent(SubmitEventDeclarationAuditEvent(pstr, error.statusCode, data, None))
    case Failure(error: HttpException) =>
      auditService.sendEvent(SubmitEventDeclarationAuditEvent(pstr, error.responseCode, data, None))
  }
}

case class SubmitEventDeclarationAuditEvent(
                                             pstr: String,
                                             status: Int,
                                             request: JsValue,
                                             response: Option[JsValue]
                                           ) extends AuditEvent {
  override def auditType: String = "AFTPost"

  override def details: JsObject = Json.obj(
    "pstr" -> pstr,
    "status" -> status.toString,
    "request" -> request,
    "response" -> response
  )
}

object SubmitEventDeclarationAuditEvent {
  implicit val formats: Format[SubmitEventDeclarationAuditEvent] = Json.format[SubmitEventDeclarationAuditEvent]
}
