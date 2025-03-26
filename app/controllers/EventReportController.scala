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

import actions.AuthAction
import models.enumeration.EventType
import models.{ReportVersion, SchemeReferenceNumber}
import play.api.Logging
import play.api.libs.json._
import play.api.mvc._
import repositories.{EventReportCacheEntry, EventReportCacheRepository}
import services.EventReportService
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http.BadRequestException
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

//noinspection ScalaStyle
@Singleton()
class EventReportController @Inject()(
                                       cc: ControllerComponents,
                                       val authConnector: AuthConnector,
                                       eventReportService: EventReportService,
                                       eventReportCacheRepository: EventReportCacheRepository,
                                       authAction: AuthAction
                                     )(implicit ec: ExecutionContext)
  extends BackendController(cc) with Logging {
  implicit val formats: Format[EventReportCacheEntry] = Json.format[EventReportCacheEntry]

  def refreshExpire(srn: SchemeReferenceNumber) = authAction(srn).async { req =>
    eventReportCacheRepository.refreshExpire(req.externalId)
      .map {
        case true => Ok("")
        case false =>
          logger.error("could not refresh expireAt")
          InternalServerError("could not refresh expireAt")
      }
      .recover {
        case e =>
          logger.error("could not refresh expireAt", e)
          InternalServerError(e.getMessage)
      }
  }

  def removeUserAnswersSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async {
    implicit request =>
      eventReportService.removeUserAnswers(request.externalId).map { _ => Ok("") }
  }

  def saveUserAnswersSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async {
    implicit request =>

      requiredHeader("pstr").map { pstr =>
        val userAnswersJson = requiredBody.validate[JsObject].getOrElse(throw new RuntimeException("Expected JsObject body"))
        val etVersionYear = (request.headers.get("eventType"), request.headers.get("version"), request.headers.get("year")) match {
          case (Some(et), Some(version), Some(year)) => Some((et, version.toInt, year.toInt))
          case _ => None
        }

        etVersionYear match {
          case Some((eventType, version, year)) =>
            EventType.getEventType(eventType) match {
              case Some(et) =>
                eventReportService.saveUserAnswers(request.externalId, pstr, et, year, version, userAnswersJson, request.getId).map(_ => Ok)
              case _ => Future.successful(NotFound(s"Bad Request: eventType ($eventType) not found"))
            }
          case _ =>
            for {
              _ <- eventReportService.saveUserAnswers(request.externalId, pstr, userAnswersJson)
              _ <- eventReportService.saveUserAnswers(request.externalId, pstr + "_original_cache", userAnswersJson)
            } yield Ok
        }
      } match {
        case Left(msg) => Future.successful(BadRequest(msg))
        case Right(result) => result
      }
  }

  def changeVersionSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async {
    implicit request =>
      (for {
        pstr <- requiredHeader("pstr")
        version <- requiredHeader("version")
        newVersion <- requiredHeader("newVersion")
      } yield {
        for {
          x <- eventReportService.changeVersion(request.externalId, pstr, version.toInt, newVersion.toInt)
          y <- eventReportService.changeVersion(request.externalId, pstr + "_original_cache", version.toInt, newVersion.toInt)
        } yield {
          (x,y) match {
            case (Some(_), Some(_)) => NoContent
            case _ => NotFound
          }
        }
      }) match {
        case Left(msg) => Future.successful(BadRequest(msg))
        case Right(result) => result
      }
  }

  def isEventDataChangedSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async {
    implicit request =>
      (for {
        pstr <- requiredHeader("pstr")
        version <- requiredHeader("version")
        eventType <- requiredHeader("eventType")
        year <- requiredHeader("year")
      } yield {
        eventReportService.isNewReportDifferentToPrevious(request.externalId, pstr, year.toInt, version.toInt, eventType)
          .map(bool => Ok(JsBoolean(bool)))
      }) match {
        case Left(msg) => Future.successful(BadRequest(msg))
        case Right(result) => result
      }
  }

  def getUserAnswersSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async {
    implicit request =>

      def process(externalId: String, psaOrPspId: String, pstr: String, optEtVersionYear: Option[(String, Int, Int)]): Future[Result] = {

        optEtVersionYear match {
          case Some((eventType, version, year)) =>
            EventType.getEventType(eventType) match {
              case Some(et) =>
                eventReportService.getUserAnswers(externalId, pstr, et, year, version, psaOrPspId)
                  .map {
                    case None => NotFound
                    case Some(jsobj) => Ok(jsobj)
                  }
              case _ => Future.successful(NotFound(s"Bad Request: eventType ($eventType) not found"))
            }
          case _ =>
            eventReportService.getUserAnswers(externalId, pstr)
              .map {
                case None => NotFound
                case Some(jsobj) => Ok(jsobj)
              }
        }
      }

      requiredHeader("pstr").map { pstr =>
        val etVersionYear = (request.headers.get("eventType"), request.headers.get("version"), request.headers.get("year")) match {
          case (Some(et), Some(version), Some(year)) => Some((et, version.toInt, year.toInt))
          case _ => None
        }
        process(request.externalId, request.getId, pstr, etVersionYear)
      } match {
        case Left(msg) => Future.successful(BadRequest(msg))
        case Right(result) => result
      }
  }

  private def requiredHeader[A](header: String)(implicit req: Request[A]):Either[String, String] = {
    req.headers.get(header).map(Right(_)).getOrElse(Left(s"Bad Request with missing header: $header"))
  }

  private def requiredBody(implicit request: Request[AnyContent]) =
    request.body.asJson.getOrElse(throw new BadRequestException("Request does not contain required Json body"))

  private def requiredBodyEither(implicit request: Request[AnyContent]) =
    request.body.asJson
      .map(Right(_)).getOrElse(Left(s"Request does not contain required Json body"))

  def getEventSummarySrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async {
    implicit request =>
      (
        for {
          pstr <- requiredHeader("pstr")
          version <- requiredHeader("reportVersionNumber")
          startDate <- requiredHeader("reportStartDate")
        } yield {
          eventReportService.getEventSummary(pstr, ("00" + version).takeRight(3), startDate, request.getId, request.externalId, request.name).map(Ok(_))
        }) match {
        case Left(msg) => Future.successful(BadRequest(msg))
        case Right(result) => result
      }
  }

  def getVersionsSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async {
    implicit request =>
      (for {
        pstr <- requiredHeader("pstr")
        startDate <- requiredHeader("startDate")
      } yield eventReportService.getVersions(pstr, startDate).map {
        data => {
          val sortedData = data.value.sortBy(y => y.as[ReportVersion].versionDetails.version).reverse
          Ok(Json.toJson(sortedData))
        }
      }) match {
        case Left(msg) => Future.successful(BadRequest(msg))
        case Right(result) => result
      }

  }

  def getOverviewSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async { implicit req =>
    val okEither = for {
      pstr <- requiredHeader("pstr")
      startDate <- requiredHeader("startDate")
      endDate <- requiredHeader("endDate")
    } yield eventReportService.getOverview(pstr, startDate, endDate).map {
      data => Ok(data)
    }

    okEither match {
      case Left(message) => Future.successful(BadRequest(message))
      case Right(resp) => resp
    }
  }

  def compileEventSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async { implicit request =>
    (for {
      pstr <- requiredHeader("pstr")
      et <- requiredHeader("eventType")
      version <- requiredHeader("version")
      currentVersion <- requiredHeader("currentVersion")
      year <- requiredHeader("year")
    } yield {
      val delete = request.headers.get("delete").contains("true")
      EventType.getEventType(et) match {
        case Some(eventType) => if(delete)
          eventReportService.deleteEvent(
            request.externalId,
            request.getId,
            pstr,
            eventType,
            year.toInt,
            version,
            currentVersion,
            None
          )
        else
          eventReportService.compileEventReport(
            request.externalId,
            request.getId,
            pstr,
            eventType,
            year.toInt,
            version,
            currentVersion
          )
        case _ => Future.failed(new BadRequestException(s"Bad Request: invalid eventType ($et)"))
      }
    }) match {
      case Left(msg) => Future.successful(BadRequest(msg))
      case Right(result) => result
    }
  }

  def deleteMemberSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async { implicit request =>
    (for {
      pstr <- requiredHeader("pstr")
      et <- requiredHeader("eventType")
      version <- requiredHeader("version")
      year <- requiredHeader("year")
      memberIdToDelete <- requiredHeader("memberIdToDelete")
      currentVersion <- requiredHeader("currentVersion")
    } yield {
      EventType.getEventType(et) match {
        case Some(eventType) => eventReportService.deleteMember(
          request.externalId,
          request.getId,
          pstr,
          eventType,
          year.toInt,
          version,
          memberIdToDelete.toInt,
          currentVersion
        )
        case _ => Future.failed(new BadRequestException(s"Bad Request: invalid eventType ($et)"))
      }
    }) match {
      case Left(msg) => Future.successful(BadRequest(msg))
      case Right(result) => result
    }
  }

  def submitEventDeclarationReportSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async {
    implicit request =>
      (for {
        pstr <- requiredHeader("pstr")
        version <- requiredHeader("version")
        userAnswersJson <- requiredBodyEither
      } yield {
        eventReportService.submitEventDeclarationReport(pstr, request.getId, userAnswersJson, version).recoverWith{
          case e: Exception =>
            logger.error(s"Error submitting event declaration report: ${e.getMessage}")
            Future.failed(new BadRequestException(s"Bad Request: ${e.getMessage}"))

        }
      }) match {
        case Left(msg) => Future.successful(BadRequest(msg))
        case Right(result) => result
      }
  }

  def submitEvent20ADeclarationReportSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async {
    implicit request =>
      (for {
        pstr <- requiredHeader("pstr")
        version <- requiredHeader("version")
        userAnswersJson <- requiredBodyEither
      } yield {
        eventReportService.submitEvent20ADeclarationReport(pstr, request.getId, userAnswersJson, version)
      }) match {
        case Left(msg) => Future.successful(BadRequest(msg))
        case Right(result) => result
      }
  }

}

