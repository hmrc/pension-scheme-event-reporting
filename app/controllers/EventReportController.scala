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
import models.{ReportVersion, SchemeReferenceNumber}
import models.enumeration.EventType
import play.api.Logging
import play.api.libs.json._
import play.api.mvc._
import repositories.EventReportCacheEntry
import services.EventReportService
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Name, ~}
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisedFunctions, Enrolment, Enrolments}
import uk.gov.hmrc.http.{UnauthorizedException, Request => _, _}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
@Singleton()
class EventReportController @Inject()(
                                       cc: ControllerComponents,
                                       val authConnector: AuthConnector,
                                       eventReportService: EventReportService,
                                       authAction: AuthAction
                                     )(implicit ec: ExecutionContext)
  extends BackendController(cc)
    with HttpErrorFunctions
    with Results
    with AuthorisedFunctions
    with Logging {
  implicit val formats: Format[EventReportCacheEntry] = Json.format[EventReportCacheEntry]

  def removeUserAnswers: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.map { case Credentials(externalId, psaPspId, _) =>
        eventReportService.removeUserAnswers(externalId)
        Ok
      }
  }

  def removeUserAnswersSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async {
    implicit request =>
      eventReportService.removeUserAnswers(request.externalId).map { _ => Ok("") }
  }

  def saveUserAnswers: Action[AnyContent] = Action.async {
    implicit request =>

      withAuth.flatMap { case Credentials(externalId, psaOrPspId, _) =>
        val userAnswersJson = requiredBody.validate[JsObject].getOrElse(throw new RuntimeException("Expected JsObject body"))
        val pstr = requiredHeaders("pstr").head
        val etVersionYear = (request.headers.get("eventType"), request.headers.get("version"), request.headers.get("year")) match {
          case (Some(et), Some(version), Some(year)) => Some((et, version.toInt, year.toInt))
          case _ => None
        }

        etVersionYear match {
          case Some((eventType, version, year)) =>
            EventType.getEventType(eventType) match {
              case Some(et) =>
                eventReportService.saveUserAnswers(externalId, pstr, et, year, version, userAnswersJson, psaOrPspId).map(_ => Ok)
              case _ => Future.failed(new NotFoundException(s"Bad Request: eventType ($eventType) not found"))
            }
          case _ =>
            for {
              _ <- eventReportService.saveUserAnswers(externalId, pstr, userAnswersJson)
              _ <- eventReportService.saveUserAnswers(externalId, pstr + "_original_cache", userAnswersJson)
            } yield Ok
        }
      }
  }

  def saveUserAnswersSrn(srn: SchemeReferenceNumber): Action[AnyContent] = authAction(srn).async {
    implicit request =>

      requiredHeader("pstr").map { pstr =>
        val userAnswersJson = requiredBody.validate[JsObject].getOrElse(throw new RuntimeException("Expected JsObject body"))
        val pstr = requiredHeaders("pstr").head
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

  def changeVersion: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.flatMap { case Credentials(externalId, _, _) =>
        val Seq(pstr, version, newVersion) = requiredHeaders("pstr", "version", "newVersion")
        for {
          x <- eventReportService.changeVersion(externalId, pstr, version.toInt, newVersion.toInt)
          y <- eventReportService.changeVersion(externalId, pstr + "_original_cache", version.toInt, newVersion.toInt)
        } yield {
          (x,y) match {
            case (Some(_), Some(_)) => NoContent
            case _ => NotFound
          }
        }

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

  def isEventDataChanged: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.flatMap { case Credentials(externalId, psaPspId, _)  =>
        val Seq(pstr, version, eventType, year) = requiredHeaders("pstr", "version", "eventType", "year")
        eventReportService.isNewReportDifferentToPrevious(externalId, pstr, year.toInt, version.toInt, psaPspId, eventType).map(bool => Ok(JsBoolean(bool)))
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
        val id = request.getId
        eventReportService.isNewReportDifferentToPrevious(request.externalId, pstr, year.toInt, version.toInt, id, eventType)
          .map(bool => Ok(JsBoolean(bool)))
      }) match {
        case Left(msg) => Future.successful(BadRequest(msg))
        case Right(result) => result
      }
  }

  def getUserAnswers: Action[AnyContent] = Action.async {
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
              case _ => Future.failed(new NotFoundException(s"Bad Request: eventType ($eventType) not found"))
            }
          case _ =>
            eventReportService.getUserAnswers(externalId, pstr)
              .map {
                case None => NotFound
                case Some(jsobj) => Ok(jsobj)
              }
        }
      }

      withAuth.flatMap { case Credentials(externalId, psaOrPspId, _) =>
        val pstr = requiredHeaders("pstr").head
        val etVersionYear = (request.headers.get("eventType"), request.headers.get("version"), request.headers.get("year")) match {
          case (Some(et), Some(version), Some(year)) => Some((et, version.toInt, year.toInt))
          case _ => None
        }
        process(externalId, psaOrPspId, pstr, etVersionYear)
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

  private def requiredHeaders(headers: String*)(implicit request: Request[AnyContent]) = {
    val headerData = headers.map(request.headers.get)
    val allHeadersDefined = headerData.forall(_.isDefined)
    if (allHeadersDefined) headerData.collect { case Some(value) => value }
    else {
      val missingHeaders = headers.zip(headerData)
      val errorString = missingHeaders.map { case (headerName, data) =>
        prettyMissingParamError(data, headerName + " missing")
      }.mkString(" ")
      throw new BadRequestException("Bad Request with missing parameters: " + errorString)
    }
  }

  private def requiredBody(implicit request: Request[AnyContent]) =
    request.body.asJson.getOrElse(throw new BadRequestException("Request does not contain required Json body"))

  private def requiredBodyEither(implicit request: Request[AnyContent]) =
    request.body.asJson
      .map(Right(_)).getOrElse(Left(s"Request does not contain required Json body"))

  def getEventSummary: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.flatMap { case Credentials(externalId, psaPspId, name)  =>
        val Seq(pstr, version, startDate) = requiredHeaders("pstr", "reportVersionNumber", "reportStartDate")
        eventReportService.getEventSummary(pstr, ("00" + version).takeRight(3), startDate, psaPspId, externalId, name).map(Ok(_))
      }
  }

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

  def getVersions: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.flatMap { _ =>
        val Seq(pstr, startDate) = requiredHeaders("pstr", "startDate")
        eventReportService.getVersions(pstr, startDate).map {
          data => {
            val sortedData = data.value.sortBy(y => y.as[ReportVersion].versionDetails.version).reverse
            Ok(Json.toJson(sortedData))
          }
        }
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

  def getOverview: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.flatMap { _ =>
        val Seq(pstr, startDate, endDate) = requiredHeaders("pstr", "startDate", "endDate")
        eventReportService.getOverview(pstr, startDate, endDate).map {
          data => Ok(data)
        }
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

  def compileEvent: Action[AnyContent] = Action.async { implicit request =>
    withAuth.flatMap { case Credentials(externalId, psaPspId, _) =>
      val Seq(pstr, et, version, currentVersion, year) = requiredHeaders("pstr", "eventType", "version", "currentVersion", "year")

      val delete = request.headers.get("delete").contains("true")
      EventType.getEventType(et) match {
        case Some(eventType) => if(delete)
          eventReportService.deleteEvent(
            externalId,
            psaPspId,
            pstr,
            eventType,
            year.toInt,
            version,
            currentVersion,
            None
          )
        else
          eventReportService.compileEventReport(
            externalId,
            psaPspId,
            pstr,
            eventType,
            year.toInt,
            version,
            currentVersion
          )
        case _ => Future.failed(new BadRequestException(s"Bad Request: invalid eventType ($et)"))
      }
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

  def deleteMember(): Action[AnyContent] = Action.async { implicit request =>
    withAuth.flatMap { case Credentials(externalId, psaPspId, _) =>
      val Seq(pstr, et, version, year, memberIdToDelete, currentVersion) =
        requiredHeaders("pstr", "eventType", "version", "year", "memberIdToDelete", "currentVersion")
      EventType.getEventType(et) match {
        case Some(eventType) => eventReportService.deleteMember(
          externalId,
          psaPspId,
          pstr,
          eventType,
          year.toInt,
          version,
          memberIdToDelete.toInt,
          currentVersion
        )
        case _ => Future.failed(new BadRequestException(s"Bad Request: invalid eventType ($et)"))
      }
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

  def submitEventDeclarationReport: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.flatMap { case Credentials(externalId, psaPspId, _) =>
        val Seq(pstr, version) = requiredHeaders("pstr", "version")
        val userAnswersJson = requiredBody
        eventReportService.submitEventDeclarationReport(pstr, psaPspId, userAnswersJson, version).recoverWith{
          case e: Exception =>
            logger.error(s"Error submitting event declaration report: ${e.getMessage}")
            Future.failed(new BadRequestException(s"Bad Request: ${e.getMessage}"))

        }
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

  def submitEvent20ADeclarationReport: Action[AnyContent] = Action.async {
    implicit request =>
      withAuth.flatMap { case Credentials(_, psaPspId, _) =>
        val Seq(pstr, version) = requiredHeaders("pstr", "version")
        val userAnswersJson = requiredBody
        eventReportService.submitEvent20ADeclarationReport(pstr, psaPspId, userAnswersJson, version)
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

  private case class Credentials(externalId: String, psaPspId: String, name: Option[Name])

  private def withAuth(implicit hc: HeaderCarrier) = {
    authorised(Enrolment("HMRC-PODS-ORG") or Enrolment("HMRC-PODSPP-ORG")).retrieve(Retrievals.externalId and Retrievals.allEnrolments and Retrievals.name) {
      case Some(externalId) ~ enrolments ~ name =>
        getPsaPspId(enrolments) match {
          case Some(psaPspId) => Future.successful(Credentials(externalId, psaPspId, name))
          case psa => Future.failed(new BadRequestException(s"Bad Request without psaPspId $psa"))
        }
      case _ =>
        Future.failed(new UnauthorizedException("Not Authorised - Unable to retrieve credentials - externalId"))
    }
  }

  private def prettyMissingParamError(param: Option[String], error: String) = if (param.isEmpty) s"$error " else ""
}

