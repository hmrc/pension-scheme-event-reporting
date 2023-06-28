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

package audit

import play.api.libs.json.{Format, JsObject, JsValue, Json}

case class CompileEventAuditEvent(psaPspIdentifier: String,
                                  pstr: String,
                                  payload: JsValue,
                                  status: Option[Int],
                                  response: Option[JsValue],
                                  errorMessage: Option[String],
                                  reportVersion: Int
                                 ) extends AuditEvent {
  override def auditType: String = "EventReportTaxReturnCompiled"

  override def details: JsObject = {
    val optStatus = status.fold[JsObject](Json.obj())(s => Json.obj("status" -> s))
    val optResponse = response.fold[JsObject](Json.obj())(s => Json.obj("response" -> s))
    val optErrorMessage = errorMessage.fold[JsObject](Json.obj())(s => Json.obj("errorMessage" -> s))

    Json.obj(
      "pspOrPsaId" -> psaPspIdentifier,
      "pstr" -> pstr,
      "payload" -> payload,
      "reportVersion" -> reportVersion
    ) ++ optStatus ++ optResponse ++ optErrorMessage
  }
}

object CompileEventAuditEvent {
  implicit val formats: Format[CompileEventAuditEvent] = Json.format[CompileEventAuditEvent]
}
