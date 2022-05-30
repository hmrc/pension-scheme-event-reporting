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

package controllers

import connectors.EventReportConnector
import play.api.Logger
import play.api.libs.json._
import play.api.mvc._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, Enrolment}
import uk.gov.hmrc.http.{UnauthorizedException, Request => _, _}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import utils.JSONPayloadSchemaValidator

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class EventReportController @Inject()(
                                       cc: ControllerComponents,
                                       eventReportConnector: EventReportConnector,
                                       val authConnector: AuthConnector,
                                       jsonPayloadSchemaValidator: JSONPayloadSchemaValidator
                                     )(implicit ec: ExecutionContext)
  extends BackendController(cc)
    with HttpErrorFunctions
    with Results
    with AuthorisedFunctions {


  private val schemaPath = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"

  private val logger = Logger(classOf[EventReportController])

  def compileEventReportSummary: Action[AnyContent] = Action.async {
    implicit request =>
      post { (pstr, userAnswersJson) =>
        logger.debug(message = s"[Compile Event Summary Report: Incoming-Payload]$userAnswersJson")
        jsonPayloadSchemaValidator.validateJsonPayload(schemaPath, userAnswersJson) match {
          case Right(true) =>
            eventReportConnector.compileEventReportSummary(pstr, userAnswersJson).map { response =>
              Ok(response.body)
            }
          case Left(errors) =>
            val allErrorsAsString = "Schema validation errors:-\n" + errors.mkString(",\n")
            throw EventReportValidationFailureException(allErrorsAsString)
          case _ => throw EventReportValidationFailureException("Schema validation failed (returned false)")
        }
      }
  }


  private def post(block: (String, JsValue) => Future[Result])
                  (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    logger.debug(message = s"[Compile Event Report: Incoming-Payload]${request.body.asJson}")

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(_) =>
        (
          request.headers.get("pstr"),
          request.body.asJson
        ) match {
          case (Some(pstr), Some(js)) =>
            block(pstr, js)
          case (pstr, jsValue) =>
            Future.failed(new BadRequestException(
              s"Bad Request without pstr ($pstr) or request body ($jsValue)"))
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }
}

case class EventReportValidationFailureException(exMessage: String) extends Exception(exMessage)

