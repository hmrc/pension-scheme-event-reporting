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

package audit

import models.enumeration.EventType
import play.api.libs.json.{Format, JsObject, JsValue, Json}

case class SubmitEventDeclarationAuditEvent(pstr: String,
                                            maybeStatus: Option[Int],
                                            request: JsValue,
                                            response: Option[JsValue],
                                            maybeErrorMessage: Option[String],
                                            reportVersion: String,
                                            maybeEventType: Option[EventType]
                                           ) extends AuditEvent {
  override def auditType: String = "EventReportTaxReturnSubmitted"

  override def details: JsObject = {
    val eventTypeJson = maybeEventType.map(v => Json.obj("eventNumber" -> v)).getOrElse(Json.obj())
    val statusJson = maybeStatus.map(v => Json.obj("status" -> v)).getOrElse(Json.obj())
    val responseJson = response.map(response => Json.obj("response" -> response)).getOrElse(Json.obj())
    val errorMessageJson = maybeErrorMessage.map(errorMessage => Json.obj("errorMessage" -> errorMessage)).getOrElse(Json.obj())

    Json.obj(
      "PensionSchemeTaxReference" -> pstr,
      "request" -> request,
      "reportVersion" -> reportVersion
    ) ++ eventTypeJson ++ statusJson ++ responseJson ++ errorMessageJson
  }
}

object SubmitEventDeclarationAuditEvent {
  implicit val formats: Format[SubmitEventDeclarationAuditEvent] = Json.format[SubmitEventDeclarationAuditEvent]
}
