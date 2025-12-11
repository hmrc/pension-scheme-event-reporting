/*
 * Copyright 2025 HM Revenue & Customs
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

package controllers

import audit.EmailAuditEvent
import models.{EmailEvents, EmailIdentifiers, Opened}
import play.api.Logger
import play.api.libs.json.{JsError, JsSuccess, JsValue}
import play.api.mvc.Results.{BadRequest, Forbidden, Ok}
import play.api.mvc.{Request, Result}
import services.AuditService
import uk.gov.hmrc.crypto.{Crypted, Decrypter, Encrypter}

import scala.concurrent.ExecutionContext

trait AuditEmailStatus {

  implicit val ec: ExecutionContext
  protected val logger: Logger
  protected val auditService: AuditService
  protected val crypto: Encrypter & Decrypter

  private val emailRegex: String = "^(?:[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"" +
    "(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")" +
    "@(?:(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?\\.)+[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?|" +
    "\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-zA-Z0-9-]*[a-zA-Z0-9]:" +
    "(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])$"

  private def decryptPsaOrPspIdAndEmail(
                                         encryptedPsaOrPspId: String,
                                         encryptedPstr: String,
                                         encryptedEmail: String
                                       ): Either[String, EmailIdentifiers] =

    val email = crypto.decrypt(Crypted(encryptedEmail)).value

    if (email.matches(emailRegex)) {
      Right(
        EmailIdentifiers(
          psaOrPspId = crypto.decrypt(Crypted(encryptedPsaOrPspId)).value,
          pstr = crypto.decrypt(Crypted(encryptedPstr)).value,
          emailAddress = email
        )
      )
    } else {
      Left(email)
    }

  protected def auditEmailStatus(
                                  submittedBy: String,
                                  requestId: String,
                                  encryptedEmail: String,
                                  encryptedPsaOrPspId: String,
                                  encryptedPstr: String,
                                  reportVersion: String
                                )(implicit request: Request[JsValue]): Result =
    decryptPsaOrPspIdAndEmail(encryptedPsaOrPspId, encryptedPstr, encryptedEmail)
      .fold(
        invalidEmailAddress =>
          Forbidden(s"Malformed email : $invalidEmailAddress"),

        emailIdentifiers =>
          request.body.validate[EmailEvents] match {

            case JsSuccess(valid, _) =>
              valid.events.filterNot(
                _.event == Opened
              ).foreach { event =>
                logger.debug(s"Email Audit event is $event")
                auditService.sendEvent(
                  EmailAuditEvent(
                    psaOrPspId = emailIdentifiers.psaOrPspId,
                    pstr = emailIdentifiers.pstr,
                    submittedBy = submittedBy,
                    emailAddress = emailIdentifiers.emailAddress,
                    event = event.event,
                    requestId = requestId,
                    reportVersion = reportVersion
                  ))(request, implicitly)
              }
              Ok

            case JsError(errors) =>
              BadRequest("Bad request received for email call back event")
          }
      )

}
