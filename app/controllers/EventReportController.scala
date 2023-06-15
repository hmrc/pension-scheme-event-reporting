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
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, Enrolment, Enrolments}
import uk.gov.hmrc.http.{UnauthorizedException, Request => _, _}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class EventReportController @Inject()(
                                       cc: ControllerComponents,
                                       val authConnector: AuthConnector,
                                       eventReportService: EventReportService
                                     )(implicit ec: ExecutionContext)
  extends BackendController(cc)
    with HttpErrorFunctions
    with Results
    with AuthorisedFunctions
    with Logging {

  def removeUserAnswers: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.map { case Credentials(externalId, psaPspId) =>
        eventReportService.removeUserAnswers(externalId)
        Ok
      }
  }

  def saveUserAnswers: Action[AnyContent] = Action.async {
    implicit request =>
      withPstrOptionEventTypeAndBodyAndExternalId { (pstr, optEventType, userAnswersJson, externalId) =>
        logger.debug(message = s"[Save Event: Incoming-Payload]$userAnswersJson")

        optEventType match {
          case Some(eventType) =>
            EventType.getEventType(eventType) match {
              case Some(et) =>
                eventReportService.saveUserAnswers(externalId, pstr, et, userAnswersJson).map(_ => Ok)
              case _ => Future.failed(new NotFoundException(s"Bad Request: eventType ($eventType) not found"))
            }
          case _ => eventReportService.saveUserAnswers(externalId, pstr, userAnswersJson).map(_ => Ok)
        }
      }
  }

  def getUserAnswers: Action[AnyContent] = Action.async {
    implicit request =>

      withAuth.flatMap { case Credentials(externalId, psaPspId) =>
        requiredHeaders("pstr") match {
          case Seq(pstr) =>
            val optEventType = request.headers.get("eventType")
            optEventType match {
              case Some(eventType) =>
                EventType.getEventType(eventType) match {
                  case Some(et) =>
                    eventReportService.getUserAnswers(externalId, pstr, et)
                      .map {
                        case None => NotFound
                        case Some(jsobj) => Ok(jsobj)
                      }
                  case _ => Future.failed(new NotFoundException(s"Bad Request: eventType ($eventType) not found"))
                }
              case None =>
                eventReportService.getUserAnswers(externalId, pstr)
                  .map {
                    case None => NotFound
                    case Some(jsobj) => Ok(jsobj)
                  }
            }

        }
      }
  }


  def getEvent: Action[AnyContent] = Action.async { implicit request =>
    withAuth.flatMap { _ =>
      requiredHeaders("pstr", "startDate", "version", "eventType") match {
        case Seq(pstr, startDate, version, eventType) =>
          val versionFormatted = ("00" + version).takeRight(3)
          EventType.getEventType(eventType) match {
            case Some(et) =>
              eventReportService.getEvent(pstr, startDate, versionFormatted, et).map {
                case Some(ee) => Ok(ee)
                case _ => NotFound
              }
            case _ => Future.failed(new BadRequestException(s"Bad Request: invalid eventType ($eventType)"))
          }
      }
    }
  }



  private def requiredHeaders(headers:String*)(implicit request: Request[AnyContent]) = {
    val headerData = headers.map(request.headers.get)
    val allHeadersDefined = headerData.forall(_.isDefined)
    if(allHeadersDefined) headerData.collect { case Some(value) => value }
    else {
      val missingHeaders = headers.zip(headerData)
      val errorString = missingHeaders.map { case (headerName, data) =>
        prettyMissingParamError(data, headerName + " missing")
      }.mkString(" ")
      throw new BadRequestException("Bad Request with missing parameters: " + errorString)
    }
  }

  def getEventSummary: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.flatMap { _ =>
        requiredHeaders("pstr", "reportVersionNumber", "reportStartDate") match {
          case Seq(pstr, version, startDate) =>
            eventReportService.getEventSummary(pstr, version, startDate).map(Ok(_))
        }
      }
  }

  def getVersions: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.flatMap { _ =>
        requiredHeaders("pstr", "reportType", "startDate") match {
          case Seq(pstr, reportType, startDate) =>
            eventReportService.getVersions(pstr, reportType, startDate).map {
              data => Ok(Json.toJson(data))
            }
        }
      }
  }

  def getOverview: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.flatMap { _ =>
        requiredHeaders("pstr", "reportType", "startDate", "endDate") match {
          case Seq(pstr, reportType, startDate, endDate) =>
            eventReportService.getOverview(pstr, reportType, startDate, endDate).map {
              data => Ok(data)
            }
        }
      }
  }


  def compileEvent: Action[AnyContent] = Action.async { implicit request =>
    withAuth.flatMap { case Credentials(externalId, psaPspId) =>
      requiredHeaders("pstr", "eventType") match {
        case Seq(pstr, et) =>
          EventType.getEventType(et) match {
            case Some(eventType) => eventReportService.compileEventReport(externalId, psaPspId, pstr, eventType)
            case _ => Future.failed(new BadRequestException(s"Bad Request: invalid eventType ($et)"))
          }
      }
    }
  }

  def submitEventDeclarationReport: Action[AnyContent] = Action.async {
    implicit request =>
      withPstrAndBody { (pstr, userAnswersJson) =>
        logger.debug(message = s"[Submit Event Declaration Report - Incoming payload]$userAnswersJson")
        eventReportService.submitEventDeclarationReport(pstr, userAnswersJson).map(_ => NoContent)
      }
  }

  def submitEvent20ADeclarationReport: Action[AnyContent] = Action.async {
    implicit request =>
      withPstrAndBody { (pstr, userAnswersJson) =>
        logger.debug(message = s"[Submit Event 20A Declaration Report - Incoming payload]$userAnswersJson")
        eventReportService.submitEvent20ADeclarationReport(pstr, userAnswersJson).map(_ => NoContent)
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

  private def withPstrAndExternalId(pstrAndExternalId: (String, String) => Future[Result])
                                            (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    logger.debug(message = "Logger message TBC")

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(externalId) =>
          request.headers.get("pstr") match {
          case Some(pstr) =>
            pstrAndExternalId(pstr, externalId)
          case _ =>
            Future.failed(new BadRequestException(s"Bad Request, no pstr in headers"))
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }

  private def withPstrOptionEventTypeAndBodyAndExternalId(block: (String, Option[String], JsValue, String) => Future[Result])
                                      (implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] = {

    logger.debug(message = s"[Compile Event Report: Incoming-Payload]${request.body.asJson}")

    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId) {
      case Some(externalId) =>
        (
          request.headers.get("pstr"),
          request.headers.get("eventType"),
          request.body.asJson,
          externalId
        ) match {
          case (Some(pstr), optET, Some(js), externalId) =>
            block(pstr, optET, js, externalId)
          case (pstr, _, jsValue, externalId) =>
            Future.failed(new BadRequestException(
              s"Bad Request without pstr ($pstr) or request body ($jsValue)"))
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }


  private def getPsaId(enrolments: Enrolments): Option[String] =
    enrolments
      .getEnrolment(key = "HMRC-PODS-ORG")
      .flatMap(_.getIdentifier("PSAID"))
      .map(_.value)

  private def getPspId(enrolments: Enrolments): Option[String] =
    enrolments
      .getEnrolment(key = "HMRC-PODSPP-ORG")
      .flatMap(_.getIdentifier("PSPID"))
      .map(_.value)

  private def getPsaPspId(enrolments: Enrolments): Option[String] =
    getPsaId(enrolments) match {
      case id@Some(_) => id
      case _ =>
        getPspId(enrolments) match {
          case id@Some(_) => id
          case _ => None
        }
    }

  private case class Credentials(externalId: String, psaPspId:String)
  private def withAuth(implicit hc: HeaderCarrier) = {
    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId and Retrievals.allEnrolments) {
      case Some(externalId) ~ enrolments =>
          getPsaPspId(enrolments) match {
            case Some(psaPspId) => Future.successful(Credentials(externalId, psaPspId))
            case psa => Future.failed(new BadRequestException(s"Bad Request without psaPspId $psa"))
          }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }
  private def prettyMissingParamError(param: Option[String], error: String) = if (param.isEmpty) s"$error " else ""
}

