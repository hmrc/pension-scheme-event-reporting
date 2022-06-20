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
import play.api.Logging
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
    with AuthorisedFunctions
    with Logging {

  private val createCompiledEventSummaryReportSchemaPath = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  private val compileEventOneReportSchemaPath = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.1.json"

  def compileEventReportSummary: Action[AnyContent] = Action.async {
    implicit request =>
      post { (pstr, userAnswersJson) =>
        logger.debug(message = s"[Compile Event Summary Report: Incoming-Payload]$userAnswersJson")
        jsonPayloadSchemaValidator.validateJsonPayload(createCompiledEventSummaryReportSchemaPath, userAnswersJson) match {
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


  def compileEventOneReport: Action[AnyContent] = Action.async {
    implicit request =>
      post { (pstr, userAnswersJson) =>
        logger.debug(message = s"[Compile Event 1 Report: Incoming-Payload]$userAnswersJson")
        jsonPayloadSchemaValidator.validateJsonPayload(compileEventOneReportSchemaPath, userAnswersJson) match {
          case Right(true) =>
            eventReportConnector.compileEventOneReport(pstr, userAnswersJson).map { response =>
              Ok(response.body)
            }
          case Left(errors) =>
            val allErrorsAsString = "Schema validation errors:-\n" + errors.mkString(",\n")
            throw EventReportValidationFailureException(allErrorsAsString)
          case _ => throw EventReportValidationFailureException("Schema validation failed (returned false)")
        }
      }
  }

  def getOverview: Action[AnyContent] = Action.async {
    implicit request =>
      withAuthAndOverviewParameters { (pstr, reportType, startDate, endDate) =>
        eventReportConnector.getOverview(pstr, reportType, startDate, endDate).map {
          data =>
            Ok(Json.toJson(data))
        }
      }
  }

  def submitEventDeclarationReport: Action[AnyContent] = Action.async {
    implicit request =>
      post { (pstr, userAnswersJson) =>
        logger.debug(message = s"[Submit Event Declaration Report - Incoming payload]$userAnswersJson")
        eventReportConnector.submitEventDeclarationReport(pstr, userAnswersJson).map { response =>
          Ok(response.body)

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

  private def withAuthAndOverviewParameters(block: (String, String, String, String) => Future[Result])
                                           (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(_) =>
        (
          request.headers.get("pstr"),
          request.headers.get("reportType"),
          request.headers.get("startDate"),
          request.headers.get("endDate")
        ) match {
          case (Some(pstr), Some(reportType), Some(startDate), Some(endDate)) =>
            block(pstr, reportType, startDate, endDate)
          case (optPstr, optReportType, optstartDate, optEndDate) =>
            val pstrMissing = optPstr.getOrElse("PSTR missing")
            val reportTypeMissing = optReportType.getOrElse("report type missing")
            val startDateMissing = optstartDate.getOrElse("start date missing")
            val endDateMissing = optEndDate.getOrElse("end date missing")
            Future.failed(new BadRequestException(s"Bad Request with missing parameters: $pstrMissing $reportTypeMissing $startDateMissing $endDateMissing "))
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }
}

case class EventReportValidationFailureException(exMessage: String) extends BadRequestException(exMessage)
