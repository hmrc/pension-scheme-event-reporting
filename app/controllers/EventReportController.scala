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

package controllers

import models.enumeration.EventType
import play.api.Logging
import play.api.libs.json._
import play.api.mvc._
import services.EventReportService
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, Enrolment}
import uk.gov.hmrc.http.{UnauthorizedException, Request => _, _}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import utils.JSONSchemaValidator

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class EventReportController @Inject()(
                                       cc: ControllerComponents,
                                       val authConnector: AuthConnector,
                                       jsonSchemaValidator: JSONSchemaValidator,
                                       eventReportService: EventReportService
                                     )(implicit ec: ExecutionContext)
  extends BackendController(cc)
    with HttpErrorFunctions
    with Results
    with AuthorisedFunctions
    with Logging {

  private val submitEventDeclarationReportSchemaPath = "/resources.schemas/api-1828-submit-event-declaration-report-request-schema-v1.0.4.json"
  private val submitEvent20ADeclarationReportSchemaPath = "/resources.schemas/api-1829-submit-event20a-declaration-report-request-schema-v1.0.0.json"

  def removeUserAnswers: Action[AnyContent] = Action.async {
    implicit request =>
      withPstr { pstr => {
        eventReportService.removeUserAnswers(pstr)
        Future.successful(Ok)
      }
    }
  }

  def saveUserAnswers: Action[AnyContent] = Action.async {
    implicit request =>
      withPstrOptionEventTypeAndBody { (pstr, optEventType, userAnswersJson) =>
        logger.debug(message = s"[Save Event: Incoming-Payload]$userAnswersJson")

        optEventType match {
          case Some(eventType) =>
            EventType.getEventType(eventType) match {
              case Some(et) =>
                eventReportService.saveUserAnswers(pstr, et, userAnswersJson).map(_ => Ok)
              case _ => Future.failed(new NotFoundException(s"Bad Request: eventType ($eventType) not found"))
            }
          case _ => eventReportService.saveUserAnswers(pstr, userAnswersJson).map(_ => Ok)
        }
      }
  }

  def getUserAnswers: Action[AnyContent] = Action.async {
    implicit request =>
      withPstrAndOptionEventType { (pstr, optEventType) =>
        optEventType match {
          case Some(eventType) =>
            EventType.getEventType(eventType) match {
              case Some(et) =>
                eventReportService.getUserAnswers(pstr, et)
                  .map {
                    case None => NotFound
                    case Some(jsobj) => Ok(jsobj)
                  }
              case _ => Future.failed(new NotFoundException(s"Bad Request: eventType ($eventType) not found"))
            }
          case None =>
            eventReportService.getUserAnswers(pstr)
              .map {
                case None => NotFound
                case Some(jsobj) => Ok(jsobj)
              }
        }
      }
  }

  def compileEvent: Action[AnyContent] = Action.async {
    implicit request =>
      withPstrAndEventType { (pstr, et) =>
        EventType.getEventType(et) match {
          case Some(eventType) => eventReportService.compileEventReport(pstr, eventType)
          case _ => Future.failed(new BadRequestException(s"Bad Request: invalid eventType ($et)"))
        }
      }
  }

  def getEvent: Action[AnyContent] = Action.async {
    implicit request =>
      withAuthAndGetEventParameters { (pstr, startDate, version, eventType) =>
        EventType.getEventType(eventType) match {
          case Some(et) =>
            eventReportService.getEvent(pstr, startDate, version, et).map {
              case Some(ee) => Ok(ee)
              case _ => NotFound
            }
          case _ => Future.failed(new BadRequestException(s"Bad Request: invalid eventType ($eventType)"))
        }
      }
  }

  def getEventSummary: Action[AnyContent] = Action.async {
    implicit request =>
      withAuthAndSummaryParameters { (pstr, version, startDate) =>
        eventReportService.getEventSummary(pstr, version, startDate).map(Ok(_))
      }
  }

  private def withAuthAndSummaryParameters(block: (String, String, String) => Future[Result])
                                          (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(_) =>
        Tuple3(
          request.headers.get("pstr"),
          request.headers.get("reportVersionNumber"),
          request.headers.get("reportStartDate")
        ) match {
          case Tuple3(Some(pstr), Some(version), Some(startDate)) =>
            block(pstr, version, startDate)
          case (optPstr, optVersion, optStartDate) =>
            val pstrMissing = prettyMissingParamError(optPstr, "PSTR missing")
            val versionMissing = prettyMissingParamError(optVersion, "version missing")
            val startDateMissing = prettyMissingParamError(optStartDate, "start date missing")
            Future.failed(new BadRequestException(s"Bad Request with missing parameters: $pstrMissing$versionMissing$startDateMissing"))
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }

  def getVersions: Action[AnyContent] = Action.async {
    implicit request =>
      withAuthAndVersionParameters { (pstr, reportType, startDate) =>
        eventReportService.getVersions(pstr, reportType, startDate).map {
          data => Ok(Json.toJson(data))
        }
      }
  }

  def getOverview: Action[AnyContent] = Action.async {
    implicit request =>
      withAuthAndOverviewParameters { (pstr, reportType, startDate, endDate) =>
        eventReportService.getOverview(pstr, reportType, startDate, endDate).map {
          data => Ok(data)
        }
      }
  }

  def submitEventDeclarationReport: Action[AnyContent] = Action.async {
    implicit request =>
      withPstrAndBody { (pstr, userAnswersJson) =>
        logger.debug(message = s"[Submit Event Declaration Report - Incoming payload]$userAnswersJson")
        Future.fromTry(jsonSchemaValidator.validatePayload(userAnswersJson, submitEventDeclarationReportSchemaPath, "submitEventDeclarationReport"))
          .flatMap { _ =>
            eventReportService.submitEventDeclarationReport(pstr, userAnswersJson).map(Ok(_))
          }
      }
  }

  def submitEvent20ADeclarationReport: Action[AnyContent] = Action.async {
    implicit request =>
      withPstrAndBody { (pstr, userAnswersJson) =>
        logger.debug(message = s"[Submit Event 20A Declaration Report - Incoming payload]$userAnswersJson")
        Future.fromTry(jsonSchemaValidator.validatePayload(userAnswersJson, submitEvent20ADeclarationReportSchemaPath, "submitEvent20ADeclarationReport"))
          .flatMap { _ =>
            eventReportService.submitEvent20ADeclarationReport(pstr, userAnswersJson).map(Ok(_))
          }
      }
  }

  private def withPstrAndBody(block: (String, JsValue) => Future[Result])
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

  private def withPstr(block: (String) => Future[Result])
                                            (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    logger.debug(message = "Logger message TBC")

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(_) =>

          request.headers.get("pstr") match {
          case Some(pstr) =>
            block(pstr)
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }

  private def withPstrOptionEventTypeAndBody(block: (String, Option[String], JsValue) => Future[Result])
                                      (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    logger.debug(message = s"[Compile Event Report: Incoming-Payload]${request.body.asJson}")

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(_) =>
        (
          request.headers.get("pstr"),
          request.headers.get("eventType"),
          request.body.asJson
        ) match {
          case (Some(pstr), optET, Some(js)) =>
            block(pstr, optET, js)
          case (pstr, _, jsValue) =>
            Future.failed(new BadRequestException(
              s"Bad Request without pstr ($pstr) or request body ($jsValue)"))
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }

  private def withPstrAndEventType(block: (String, String) => Future[Result])
                                  (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    logger.debug(message = s"[Compile Event Report: Incoming-Payload]${request.body.asJson}")

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(_) =>
        (
          request.headers.get("pstr"),
          request.headers.get("eventType")
        ) match {
          case (Some(pstr), Some(et)) =>
            block(pstr, et)
          case (pstr, et) =>
            Future.failed(new BadRequestException(
              s"Bad Request without pstr ($pstr) or eventType ($et)"))
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }

  private def withPstrAndOptionEventType(block: (String, Option[String]) => Future[Result])
                                        (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    logger.debug(message = s"[Compile Event Report: Incoming-Payload]${request.body.asJson}")

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(_) =>
        (
          request.headers.get("pstr"),
          request.headers.get("eventType")
        ) match {
          case (Some(pstr), et) =>
            block(pstr, et)
          case (pstr, _) =>
            Future.failed(new BadRequestException(
              s"Bad Request without pstr ($pstr)"))
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }

  private def withAuthAndGetEventParameters(block: (String, String, String, String) => Future[Result])
                                           (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(_) =>
        (
          request.headers.get("pstr"),
          request.headers.get("startDate"),
          request.headers.get("version"),
          request.headers.get("eventType")
        ) match {
          case (Some(pstr), Some(startDate), Some(version), Some(eventType)) =>
            val versionFormatted = ("00" + version).takeRight(3)
            block(pstr, startDate, versionFormatted, eventType)
          case (optPstr, optStartDate, optVersion, optEventType) =>
            val pstrMissing = prettyMissingParamError(optPstr, "PSTR missing")
            val startDateMissing = prettyMissingParamError(optStartDate, "start date missing")
            val versionMissing = prettyMissingParamError(optVersion, "version missing")
            val eventTypeMissing = prettyMissingParamError(optEventType, "event type missing")
            Future.failed(new BadRequestException(
              s"Bad Request with missing parameters: $pstrMissing $eventTypeMissing $startDateMissing $versionMissing"))
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

