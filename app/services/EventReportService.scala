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

package services


import com.google.inject.{Inject, Singleton}
import connectors.EventReportConnector
import models.enumeration.ApiType._
import models.enumeration.EventType._
import models.enumeration.{ApiType, EventType}
import models.{EROverview, ERVersion, EventDataIdentifier}
import org.mongodb.scala.result
import play.api.Logging
import play.api.http.Status.NOT_IMPLEMENTED
import play.api.libs.json.JsResult.toTry
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc.{RequestHeader, Result}
import repositories.EventReportCacheRepository
import transformations.ETMPToFrontEnd.{API1831, API1832, API1833, API1834}
import transformations.UserAnswersToETMP._
import uk.gov.hmrc.http.{BadRequestException, ExpectationFailedException, HeaderCarrier, HttpResponse}
import utils.JSONSchemaValidator

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


@Singleton()
class EventReportService @Inject()(eventReportConnector: EventReportConnector,
                                   eventReportCacheRepository: EventReportCacheRepository,
                                   jsonPayloadSchemaValidator: JSONSchemaValidator,
                                   compilePayloadService: CompilePayloadService
                                  ) extends Logging {
  private final val SchemaPath1826 = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  private final val SchemaPath1827 = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.4.json"
  private final val SchemaPath1830 = "/resources.schemas/api-1830-create-compiled-member-event-report-request-schema-v1.0.7.json"
  private final val SchemaPath1828 = "/resources.schemas/api-1828-submit-event-declaration-report-request-schema-v1.0.4.json"
  private final val SchemaPath1829 = "/resources.schemas/api-1829-submit-event20a-declaration-report-request-schema-v1.0.0.json"

  private case class APIProcessingInfo(apiType: ApiType,
                                       readsForTransformation: Reads[JsObject],
                                       schemaPath: String,
                                       connectToAPI: (String, String, JsValue, String) => Future[HttpResponse]
                                      )

  private def apiProcessingInfo(eventType: EventType, pstr: String)
                               (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Option[APIProcessingInfo] = {
    EventType.postApiTypeByEventType(eventType) flatMap {
      case Api1826 =>
        Some(APIProcessingInfo(Api1826, API1826.transformToETMPData, SchemaPath1826, eventReportConnector.compileEventReportSummary))
      case Api1827 =>
        Some(APIProcessingInfo(Api1827, API1827.transformToETMPData, SchemaPath1827, eventReportConnector.compileEventOneReport))
      case Api1830 =>
        Some(APIProcessingInfo(Api1830, API1830.transformToETMPData(eventType, pstr), SchemaPath1830,
          eventReportConnector.compileMemberEventReport))
      case _ => None
    }
  }

  def saveUserAnswers(externalId: String, pstr: String, eventType: EventType, year: Int, version: Int, userAnswersJson: JsObject)
                     (implicit ec: ExecutionContext): Future[Unit] =
    eventReportCacheRepository.upsert(pstr, EventDataIdentifier(eventType, year, version, externalId), userAnswersJson)

  def saveUserAnswers(externalId: String, pstr: String, userAnswersJson: JsValue)(implicit ec: ExecutionContext): Future[Unit] =
    eventReportCacheRepository.upsert(externalId, pstr, userAnswersJson)

  def changeVersion(externalId: String, pstr: String, currentVersion: Int, newVersion: Int)
                   (implicit ec: ExecutionContext): Future[Option[result.UpdateResult]] =
    eventReportCacheRepository.changeVersion(externalId, pstr, currentVersion, newVersion)

  def removeUserAnswers(externalId: String)(implicit ec: ExecutionContext): Future[Unit] =
    eventReportCacheRepository.removeAllOnSignOut(externalId)

  def getUserAnswers(externalId: String, pstr: String, eventType: EventType, year: Int, version: Int)
                    (implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[JsObject]] =
    eventReportCacheRepository.getUserAnswers(externalId, pstr, Some(EventDataIdentifier(eventType, year, version, externalId))).flatMap {
      case x@Some(_) =>
        Future.successful(x)
      case None =>
        val startDate = year.toString + "-04-06"
        getEvent(pstr, startDate, version, eventType).flatMap {
          case None =>
            Future.successful(None)
          case optUAData@Some(userAnswersDataToStore) =>
            for {
              _ <- saveUserAnswers(externalId, pstr + "_original_cache", eventType, year, version, userAnswersDataToStore)
              resp <- saveUserAnswers(externalId, pstr, eventType, year, version, userAnswersDataToStore).map(_ => optUAData)
            } yield resp
        }
    }


  def getUserAnswers(externalId: String, pstr: String)(implicit ec: ExecutionContext): Future[Option[JsObject]] =
    eventReportCacheRepository.getUserAnswers(externalId, pstr, None)

  private case class MemberChangeInfo(amendedVersion: Int, status: MemberStatus)

  private trait MemberStatus {
    def name:String
  }

  private case class New() extends MemberStatus {
    def name: String = "New"
  }

  private case class Deleted() extends MemberStatus {
    def name: String = "Deleted"
  }

  private case class Changed() extends MemberStatus {
    def name: String = "Changed"
  }

  private def stringToMemberStatus(memberStatus:String): MemberStatus = memberStatus match {
    case "New" => New()
    case "Deleted" => Deleted()
    case "Changed" => Changed()
    case memberStatus => throw new RuntimeException("Unknown member status: " + memberStatus)
  }

  private def generateMemberChangeInfo(oldMember: Option[JsObject],
                                  newMember: JsObject,
                                  currentVersion: Int): MemberChangeInfo = {

    def noVersion(obj:JsObject) = obj - "amendedVersion" - "memberStatus"

    def version(obj: JsObject) = obj.value.get("amendedVersion").map(_.as[String].toInt)

    def status(obj: JsObject) = obj.value.get("memberStatus").map(_.as[String]).map(stringToMemberStatus)

    oldMember.map { oldMember =>
      val oldMemberNoVersion = noVersion(oldMember)
      val newMemberNoVersion = noVersion(newMember)
      val memberChanged = oldMemberNoVersion != newMemberNoVersion
      val oldMemberVersion = version(oldMember).getOrElse(currentVersion)
      val hasSameVersion = version(newMember).contains(currentVersion)
      val oldMemberStatus = status(oldMember).getOrElse(New())
      
      (hasSameVersion, memberChanged, oldMemberStatus) match {
        case (false, false, oldMemberStatus) => MemberChangeInfo(oldMemberVersion, oldMemberStatus)
        case (true, _, New()) => MemberChangeInfo(oldMemberVersion, New())
        case (false, true, _) => MemberChangeInfo(currentVersion, Changed())
        case (true, false, oldMemberStatus)  => MemberChangeInfo(oldMemberVersion, oldMemberStatus)
      }
    }.getOrElse(
      MemberChangeInfo(currentVersion, New())
    )
  }

  //scalastyle:off method.length
  private def memberChangeInfoTransformation(oldUserAnswers: Option[JsObject],
                                             newUserAnswers: JsObject,
                                             eventType: EventType,
                                             pstr: String,
                                             currentVersion: Int)
                                        (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader):JsObject = {

    def newMembersWithChangeInfo(getMemberDetails: JsObject => scala.collection.IndexedSeq[JsObject]) = {
      val newMembers = getMemberDetails(newUserAnswers)

      newMembers.zipWithIndex.map { case (newMemberDetail, index) =>
        val oldMemberDetails = oldUserAnswers.map(getMemberDetails).getOrElse(Seq())
        val oldMemberDetail = Try(oldMemberDetails(index)).toOption
        val newMemberChangeInfo = generateMemberChangeInfo(
          oldMemberDetail,
          newMemberDetail,
          currentVersion
        )

        newMemberDetail +
          ("amendedVersion", JsString(("00" + newMemberChangeInfo.amendedVersion.toString).takeRight(3))) +
          ("memberStatus", JsString(newMemberChangeInfo.status.name))
      }
    }

    apiProcessingInfo(eventType, pstr) match {
      case Some(APIProcessingInfo(apiType, _, _, _)) =>
        apiType match {
          case ApiType.Api1827 =>
            def getMemberDetails(userAnswers: JsObject) =
              (userAnswers \ "event1" \ "membersOrEmployers").as[JsArray].value.map(_.as[JsObject])

            val event1 = ((newUserAnswers \ "event1").as[JsObject] - "membersOrEmployers") +
              ("membersOrEmployers", Json.toJson(newMembersWithChangeInfo(getMemberDetails)))

            newUserAnswers - "event1" + ("event1", event1)
          case ApiType.Api1830 =>
            def getMemberDetails(userAnswers: JsObject) = {
              (userAnswers \ ("event" + eventType.toString) \ "members").as[JsArray].value.map(_.as[JsObject])
            }

            val event = ((newUserAnswers \ ("event" + eventType.toString)).as[JsObject] - "members") +
              ("members", Json.toJson(newMembersWithChangeInfo(getMemberDetails)))
            newUserAnswers - ("event" + eventType.toString) + ("event" + eventType.toString, event)

          case _ => newUserAnswers
        }
      case _ => throw new RuntimeException("Unknown API type for eventType - " + eventType)
    }
  }

  def compileEventReport(externalId: String, psaPspId: String, pstr: String, eventType: EventType, year: Int, version: String)
                        (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[Result] = {
    apiProcessingInfo(eventType, pstr) match {
      case Some(APIProcessingInfo(apiType, reads, schemaPath, connectToAPI)) =>
        val resp = for {
          newUserAnswers <- eventReportCacheRepository.getUserAnswers(externalId, pstr, Some(EventDataIdentifier(eventType, year, version.toInt, externalId)))
          oldUserAnswers <- eventReportCacheRepository.getUserAnswers(
            externalId, pstr + "_original_cache", Some(EventDataIdentifier(eventType, year, version.toInt, externalId))
          )
        } yield {
          (newUserAnswers, oldUserAnswers) match {
            case (Some(newUserAnswers), oldUserAnswers) =>
              val header = Json.obj(
                "taxYear" -> year.toString
              )

              val data = memberChangeInfoTransformation(oldUserAnswers, newUserAnswers, eventType, pstr, version.toInt)

              val fullData = data ++ header

              for {
                transformedData <- Future.fromTry(toTry(fullData.validate(reads)))
                collatedData <- compilePayloadService.collatePayloadsAndUpdateCache(
                  pstr,
                  year,
                  version,
                  apiType,
                  eventType,
                  transformedData)
                _ <- Future.fromTry(jsonPayloadSchemaValidator.validatePayload(collatedData, schemaPath, apiType.toString))
                response <- connectToAPI(psaPspId, pstr, collatedData, version)
              } yield {
                response.status match {
                  case NOT_IMPLEMENTED => BadRequest(s"Not implemented - event type $eventType")
                  case _ =>
                    logger.debug(s"SUCCESSFUL SUBMISSION TO COMPILE API $apiType: $transformedData")
                    NoContent
                }
              }
            case _ => Future.successful(NotFound)
          }
        }
        resp.flatten
      case _ => Future.successful(BadRequest(s"Compile unimplemented for event type $eventType"))
    }
  }

  private val api1832Events: List[EventType] = List(Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A, Event22, Event23, Event24)
  private val api1834Events: List[EventType] = List(WindUp, Event10, Event18, Event13, Event20, Event11, Event12, Event14, Event19)

  private def transformOrException(data: JsObject, reads: Reads[JsObject]): Option[JsObject] = {
    data.validate(reads) match {
      case JsSuccess(transformedData, _) => Some(transformedData)
      case JsError(e) => throw JsResultException(e)
    }
  }

  private def transformGETResponse(data: JsObject, eventType: EventType): Option[JsObject] = {
    eventType match {
      case Event1 => transformOrException(data, API1833.rds1833Api)
      case Event20A => transformOrException(data, API1831.rds1831Api)
      case evType1832 if api1832Events.contains(evType1832) => transformOrException(data, API1832.rds1832Api(evType1832))
      case evType1834 if api1834Events.contains(evType1834) => transformOrException(data, API1834.reads(evType1834))
      case _ => throw new RuntimeException(s"No transformation available for event type $eventType")
    }
  }

  def getEvent(pstr: String, startDate: String, version: Int, eventType: EventType)
              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Option[JsObject]] = {
    eventReportConnector.getEvent(pstr, startDate, version.toString, Some(eventType)).flatMap {
      case Some(data) =>
        val jsDataOpt = transformGETResponse(data, eventType)
        Future.successful(jsDataOpt)
      case _ =>
        Future.successful(None)
    }
  }

  def getEventSummary(pstr: String, version: String, startDate: String)
                     (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsArray] = {

    val resp1834Seq = eventReportConnector.getEvent(pstr, startDate, version, None).map { etmpJsonOpt =>
      etmpJsonOpt.map { etmpJson =>
        etmpJson.transform(transformations.ETMPToFrontEnd.API1834Summary.rdsFor1834) match {
          case JsSuccess(seqOfEventTypes, _) =>
            seqOfEventTypes
          case JsError(errors) => throw JsResultException(errors)
        }
      }.getOrElse(JsArray())
    }.recover { error =>
      logger.error(error.getMessage, error)
      JsArray()
    }

    val resp1831Seq = eventReportConnector.getEvent(pstr, startDate, version, Some(Event20A)).map { etmpJsonOpt =>
      etmpJsonOpt.map { etmpJson =>
        etmpJson.transform(transformations.ETMPToFrontEnd.API1834Summary.rdsFor1831) match {
          case JsSuccess(seqOfEventTypes, _) => seqOfEventTypes
          case JsError(errors) => throw JsResultException(errors)
        }
      }.getOrElse(JsArray())
    } recover { error =>
      logger.error(error.getMessage, error)
      JsArray()
    }

    Future.sequence(Set(resp1834Seq, resp1831Seq)) map { x =>
      x reduce (_ ++ _)
    }
  }

  def getVersions(pstr: String, startDate: String)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Seq[ERVersion]] = {
    val erVersions = eventReportConnector.getVersions(pstr, reportType = "ER", startDate)
    val er20AVersions = eventReportConnector.getVersions(pstr, reportType = "ER20A", startDate)
    Future.sequence(Seq(erVersions, er20AVersions)).map(_.flatten)
  }

  def getOverview(pstr: String, startDate: String, endDate: String)
                 (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsValue] = {
    val erOverview = eventReportConnector.getOverview(pstr, reportType = "ER", startDate, endDate)
    val er20AOverview = eventReportConnector.getOverview(pstr, reportType = "ER20A", startDate, endDate)

    val combinedEROverview = erOverview.flatMap { a =>
      er20AOverview.map { b =>
        EROverview.combine(a, b)
      }
    }
    combinedEROverview.map(data => Json.toJson(data))
  }

  private def validatePayloadAgainstSchema(payload: JsObject, schemaPath: String, eventName: String)
                                          (implicit ec: ExecutionContext): Future[Unit] = {
    Future.fromTry(jsonPayloadSchemaValidator.validatePayload(payload, schemaPath, eventName)).map(_ => (): Unit)
  }

  def submitEventDeclarationReport(pstr: String, userAnswersJson: JsValue, version: String)
                                  (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[Unit] = {

    def recoverAndValidatePayload(transformed1828Payload: JsObject): Future[Unit] = {
      val recoveredConnectorCallForAPI1828 = eventReportConnector
        .submitEventDeclarationReport(pstr, transformed1828Payload, version).map(_.json.as[JsObject]).recover {
        case _: BadRequestException =>
          throw new ExpectationFailedException("Nothing to submit")
      }
      for {
        _ <- recoveredConnectorCallForAPI1828
        _ <- validatePayloadAgainstSchema(transformed1828Payload, SchemaPath1828, "submitEventDeclarationReport")
      } yield {
        ()
      }
    }

    for {
      transformed1828Payload <- Future.fromTry(toTry(userAnswersJson.transform(API1828.transformToETMPData)))
      _ <- recoverAndValidatePayload(transformed1828Payload)
    } yield {
      ()
    }
  }


  def submitEvent20ADeclarationReport(pstr: String, userAnswersJson: JsValue, version: String)
                                     (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[Unit] = {


    def recoverAndValidatePayload(transformed1829Payload: JsObject): Future[Unit] = {

      val recoveredConnectorCallForAPI1829 =
        eventReportConnector.submitEvent20ADeclarationReport(pstr, transformed1829Payload, version).map(_.json.as[JsObject]).recover {
          case _: BadRequestException =>
            throw new ExpectationFailedException("Nothing to submit")
        }
      for {
        _ <- recoveredConnectorCallForAPI1829
        _ <- validatePayloadAgainstSchema(transformed1829Payload, SchemaPath1829, "submitEvent20ADeclarationReport")
      } yield {
        ()
      }
    }

    for {
      transformed1829Payload <- Future.fromTry(toTry(userAnswersJson.transform(API1829.transformToETMPData)))
      _ <- recoverAndValidatePayload(transformed1829Payload)
    } yield {
      ()
    }
  }
}