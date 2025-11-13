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

package controllers

import audit.EmailAuditEvent
import com.google.inject.Inject
import models.{EmailEvents, Opened}
import play.api.Logger
import play.api.libs.json.JsValue
import play.api.mvc._
import services.AuditService
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions}
import uk.gov.hmrc.crypto.{Crypted}
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.ApplicationCrypto
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext


class EmailResponseController @Inject()(
                                         auditService: AuditService,
                                         cc: ControllerComponents,
                                         crypto: ApplicationCrypto,
                                         parser: PlayBodyParsers,
                                         val authConnector: AuthConnector
                                       )(implicit ec: ExecutionContext) extends BackendController(cc) with AuthorisedFunctions {
  private val logger = Logger(classOf[EmailResponseController])

  def sendAuditEvents(
                       submittedBy: String,
                       requestId: String,
                       email: String,
                       encryptedPsaOrPspId: String,
                       encryptedPstr: String,
                       reportVersion: String): Action[JsValue] = Action(parser.tolerantJson) {
    implicit request =>
      decryptPsaOrPspIdAndEmail(encryptedPsaOrPspId, encryptedPstr, email) match {
        case Right(Tuple3(psaOrPspId, pstr, emailAddress)) =>
          request.body.validate[EmailEvents].fold(
            _ => BadRequest("Bad request received for email call back event"),
            valid => {
              valid.events.filterNot(
                _.event == Opened
              ).foreach { event =>
                logger.debug(s"Email Audit event is $event")
                auditService.sendEvent(EmailAuditEvent(psaOrPspId, pstr, submittedBy, emailAddress, event.event, requestId, reportVersion))(request, implicitly)
              }
              Ok
            }
          )

        case Left(result) => result
      }
  }

  private def decryptPsaOrPspIdAndEmail(encryptedPsaOrPspId: String, encryptedPstr: String, email: String): Either[Result, (String, String, String)] = {
    val psaOrPspId = crypto.QueryParameterCrypto.decrypt(Crypted(encryptedPsaOrPspId)).value
    val pstr = crypto.QueryParameterCrypto.decrypt(Crypted(encryptedPstr)).value
    val emailAddress = crypto.QueryParameterCrypto.decrypt(Crypted(email)).value
    val emailRegex: String = "^(?:[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"" +
      "(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")" +
      "@(?:(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?\\.)+[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?|" +
      "\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-zA-Z0-9-]*[a-zA-Z0-9]:" +
      "(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])$"

    try {
      require(emailAddress.matches(emailRegex))
      Right(Tuple3(psaOrPspId, pstr, emailAddress))
    } catch {
      case _: IllegalArgumentException => Left(Forbidden(s"Malformed email : $emailAddress"))
    }
  }
}
