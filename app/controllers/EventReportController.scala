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
import models.enumeration.EventType
import play.api.Logging
import play.api.libs.json._
import play.api.mvc._
import repositories.EventReportCacheRepository
import services.EventReportService
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
                                       eventReportCacheRepository: EventReportCacheRepository,
                                       jsonPayloadSchemaValidator: JSONPayloadSchemaValidator,
                                       eventReportService: EventReportService
                                     )(implicit ec: ExecutionContext)
  extends BackendController(cc)
    with HttpErrorFunctions
    with Results
    with AuthorisedFunctions
    with Logging {

  private val submitEventDeclarationReportSchemaPath = "/resources.schemas/api-1828-submit-event-declaration-report-request-schema-v1.0.0.json"

  def saveEvent: Action[AnyContent] = Action.async {
    implicit request =>
      postWithEventType { (pstr, eventType, userAnswersJson) =>
        logger.debug(message = s"[Save Event: Incoming-Payload]$userAnswersJson")
        EventType.getEventType(eventType) match {
          case Some(event) =>
            EventType.getApiTypeByEventType(event) match {
              // TODO: Have discussion on potential for overwriting in Mongo.
              case Some(apiType) => eventReportCacheRepository.upsert(pstr, apiType, userAnswersJson)
                .map(_ => Created)
              case _ => Future.failed(new NotFoundException(s"Not Found: ApiType not found for eventType ($eventType)"))
            }
          case _ => Future.failed(new BadRequestException(s"Bad Request: invalid eventType ($eventType)"))
        }
      }
  }

  def compileEvent: Action[AnyContent] = Action.async {
    implicit request =>
      post { (pstr, userAnswersJson) =>
        logger.debug(message = s"[Compile Event: Incoming-Payload]$userAnswersJson")
        eventReportService.compileEventReport(pstr, userAnswersJson)
      }
  }

  def getVersions: Action[AnyContent] = Action.async {
    implicit request =>
      withAuthAndVersionParameters { (pstr, reportType, startDate) =>
        eventReportConnector.getVersions(pstr, reportType, startDate).map {
          data =>
            Ok(Json.toJson(data))
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
        jsonPayloadSchemaValidator.validateJsonPayload(submitEventDeclarationReportSchemaPath, userAnswersJson) match {
          case Right(true) =>
            eventReportConnector.submitEventDeclarationReport(pstr, userAnswersJson).map { response =>
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

  private def postWithEventType(block: (String, String, JsValue) => Future[Result])
                               (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    logger.debug(message = s"[Compile Event Report: Incoming-Payload]${request.body.asJson}")

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(_) =>
        (
          request.headers.get("pstr"),
          request.headers.get("eventType"),
          request.body.asJson
        ) match {
          case (Some(pstr), Some(et), Some(js)) => block(pstr, et, js)
          case (pstr, et, jsValue) =>
            Future.failed(new BadRequestException(
              s"Bad Request without pstr ($pstr) or eventType ($et) or request body ($jsValue)"))
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
          case (optPstr, optReportType, optStartDate, optEndDate) =>
            val pstrMissing = prettyMissingParamError(optPstr, "PSTR missing")
            val reportTypeMissing = prettyMissingParamError(optReportType, "report type missing")
            val startDateMissing = prettyMissingParamError(optStartDate, "start date missing")
            val endDateMissing = prettyMissingParamError(optEndDate, "end date missing")
            Future.failed(new BadRequestException(s"Bad Request with missing parameters: $pstrMissing$reportTypeMissing$startDateMissing$endDateMissing"))
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }

  private def withAuthAndVersionParameters(block: (String, String, String) => Future[Result])
                                          (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(_) =>
        (
          request.headers.get("pstr"),
          request.headers.get("reportType"),
          request.headers.get("startDate")
        ) match {
          case (Some(pstr), Some(reportType), Some(startDate)) =>
            block(pstr, reportType, startDate)
          case (optPstr, optReportType, optStartDate) =>
            val pstrMissing = prettyMissingParamError(optPstr, "PSTR missing")
            val reportTypeMissing = prettyMissingParamError(optReportType, "report type missing")
            val startDateMissing = prettyMissingParamError(optStartDate, "start date missing")
            Future.failed(new BadRequestException(s"Bad Request for version with missing parameters: $pstrMissing $reportTypeMissing $startDateMissing"))
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }

  private def prettyMissingParamError(param: Option[String], error: String) = if (param.isEmpty) s"$error " else ""
}

case class EventReportValidationFailureException(exMessage: String) extends BadRequestException(exMessage)
