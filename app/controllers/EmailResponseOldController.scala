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

import com.google.inject.Inject
import play.api.Logger
import play.api.libs.json.JsValue
import play.api.mvc.*
import services.AuditService
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions}
import uk.gov.hmrc.crypto.{ApplicationCrypto, Decrypter, Encrypter}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext


class EmailResponseOldController @Inject()(
                                         val auditService: AuditService,
                                         cc: ControllerComponents,
                                         applicationCrypto: ApplicationCrypto,
                                         parser: PlayBodyParsers,
                                         val authConnector: AuthConnector
                                       )(implicit val ec: ExecutionContext) extends BackendController(cc) with AuthorisedFunctions with AuditEmailStatus {

  override protected val logger: Logger = Logger(classOf[EmailResponseOldController])
  override protected val crypto: Encrypter & Decrypter = applicationCrypto.QueryParameterCrypto

  def sendAuditEvents(
                       submittedBy: String,
                       requestId: String,
                       encryptedEmail: String,
                       encryptedPsaOrPspId: String,
                       encryptedPstr: String,
                       reportVersion: String): Action[JsValue] =
    Action(parser.tolerantJson) {
      implicit request =>
        auditEmailStatus(submittedBy, requestId, encryptedEmail, encryptedPsaOrPspId, encryptedPstr, reportVersion)
  }
}
