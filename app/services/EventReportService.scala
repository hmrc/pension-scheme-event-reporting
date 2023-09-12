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
import models.MemberChangeInfo.Deleted
import models.enumeration.ApiType._
import models.enumeration.EventType._
import models.enumeration.{ApiType, EventType}
import models.{EROverview, EventDataIdentifier}
import org.mongodb.scala.result
import play.api.Logging
import play.api.http.Status.NOT_IMPLEMENTED
import play.api.libs.json.JsResult.toTry
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc.{RequestHeader, Result}
import repositories.{DeclarationLockRepository, EventReportCacheRepository}
import transformations.ETMPToFrontEnd._
import transformations.UserAnswersToETMP._
import uk.gov.hmrc.http.{BadRequestException, ExpectationFailedException, HeaderCarrier, HttpResponse}
import utils.JSONSchemaValidator

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}


@Singleton()
class EventReportService @Inject()(eventReportConnector: EventReportConnector,
                                   eventReportCacheRepository: EventReportCacheRepository,
                                   declarationLockRepository: DeclarationLockRepository,
                                   jsonPayloadSchemaValidator: JSONSchemaValidator,
                                   compilePayloadService: CompilePayloadService,
                                   memberChangeInfoService: MemberChangeInfoService
                                  ) extends Logging {

  private final val SchemaPath1826 = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.1.0.json"
  private final val SchemaPath1827 = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.4.json"
  private final val SchemaPath1830 = "/resources.schemas/api-1830-create-compiled-member-event-report-request-schema-v1.0.7.json"
  private final val SchemaPath1828 = "/resources.schemas/api-1828-submit-event-declaration-report-request-schema-v1.0.4.json"
  private final val SchemaPath1829 = "/resources.schemas/api-1829-submit-event20a-declaration-report-request-schema-v1.0.0.json"

  private case class APIProcessingInfo(apiType: ApiType,
                                       readsForTransformation: Reads[JsObject],
                                       schemaPath: String,
                                       connectToAPI: (String, String, JsValue, String) => Future[HttpResponse]
                                      )

  private def apiProcessingInfo(eventType: EventType, pstr: String, delete: Boolean)
                               (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Option[APIProcessingInfo] = {
    EventType.postApiTypeByEventType(eventType) flatMap {
      case Api1826 =>
        Some(APIProcessingInfo(
          Api1826,
          API1826.transformToETMPData(eventType, delete),
          SchemaPath1826,
          eventReportConnector.compileEventReportSummary
        ))
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

  //scalastyle:off method.length
  def memberChangeInfoTransformation(oldUserAnswers: Option[JsObject],
                                             newUserAnswers: JsObject,
                                             eventType: EventType,
                                             pstr: String,
                                             currentVersion: Int,
                                             delete: Boolean)
                                            (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): JsObject = {

    def newMembersWithChangeInfo(getMemberDetails: JsObject => Option[scala.collection.IndexedSeq[JsObject]]) = {
      val newMembers = getMemberDetails(newUserAnswers)

      newMembers
        .getOrElse(throw new RuntimeException("new member not available"))
        .zipWithIndex.flatMap { case (newMemberDetail, index) =>
        val oldMemberDetails = oldUserAnswers.flatMap(getMemberDetails).getOrElse(Seq())
        val oldMemberDetail = Try(oldMemberDetails(index)).toOption
        memberChangeInfoService.generateMemberChangeInfo(
          oldMemberDetail,
          newMemberDetail,
          currentVersion
        ).map { newMemberChangeInfo =>
          val amendedMemberDetails = newMemberDetail +
            ("amendedVersion", JsString(("00" + newMemberChangeInfo.amendedVersion.toString).takeRight(3))) +
            ("memberStatus", JsString(newMemberChangeInfo.status.name))

          if(newMemberChangeInfo.amendedVersion == currentVersion) amendedMemberDetails - "amendedVersion"
          else amendedMemberDetails
        }
      }
    }

    apiProcessingInfo(eventType, pstr, delete) match {
      case Some(APIProcessingInfo(apiType, _, _, _)) =>
        apiType match {
          case ApiType.Api1827 =>
            def getMemberDetails(userAnswers: JsObject) =
              Try((userAnswers \ "event1" \ "membersOrEmployers").as[JsArray].value.map(_.as[JsObject])).toOption

            val event1 = {
              Try(
                ((newUserAnswers \ "event1").as[JsObject] - "membersOrEmployers") +
                  ("membersOrEmployers", Json.toJson(newMembersWithChangeInfo(getMemberDetails)))
              ) match {
                case Failure(exception) =>
                  logger.warn("Could not get member details for event 1", exception)
                  Json.toJson(IndexedSeq(JsObject(Seq())))
                case Success(value) => value
              }
            }

            (newUserAnswers - "event1") + ("event1", event1)
          case ApiType.Api1830 =>
            def getMemberDetails(userAnswers: JsObject) = {
              Try(
                (userAnswers \ ("event" + eventType.toString) \ "members").as[JsArray].value.map(_.as[JsObject])
              ) match {
                case Failure(exception) =>
                  logger.warn("Could not get member details", exception)
                  None
                case Success(value) => Some(value)
              }
            }

            val event = ((newUserAnswers \ ("event" + eventType.toString)).as[JsObject] - "members") +
              ("members", Json.toJson(newMembersWithChangeInfo(getMemberDetails)))
            newUserAnswers - ("event" + eventType.toString) + ("event" + eventType.toString, event)

          case _ => newUserAnswers
        }
      case _ => throw new RuntimeException("Unknown API type for eventType - " + eventType)
    }
  }

  def compileEventReport(externalId: String,
                         psaPspId: String,
                         pstr: String,
                         eventType: EventType,
                         year: Int,
                         version: String,
                         currentVersion: String,
                         deleteEvent: Boolean = false)
                        (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[Result] = {
    apiProcessingInfo(eventType, pstr, deleteEvent) match {
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

              val data = memberChangeInfoTransformation(oldUserAnswers,
                compilePayloadService.addRecordVersionToUserAnswersJson(eventType, version.toInt, newUserAnswers), eventType, pstr, version.toInt, deleteEvent)

              val fullData = data ++ header
              for {
                transformedData <- Future.fromTry(toTry(fullData.validate(reads)))
                collatedData <- compilePayloadService.collatePayloadsAndUpdateCache(
                  pstr,
                  year,
                  currentVersion,
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

  def deleteMember(externalId: String,
                   psaPspId: String,
                   pstr: String,
                   eventType: EventType,
                   year: Int,
                   version: String,
                   memberIdToDelete: Int,
                   currentVersion: String)
                  (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[Result] = {

    def memberTransform(members: Seq[JsObject]): Seq[JsObject] = {
      val member = Try(members(memberIdToDelete)) match {
        case Failure(exception) => throw new RuntimeException("Member does not exist", exception)
        case Success(member) => member + ("memberStatus", JsString(Deleted().name))
      }
      members.updated(memberIdToDelete, member)
    }

    getUserAnswers(externalId, pstr, eventType, year, version.toInt).flatMap {
      case Some(ua) => saveUserAnswers(externalId, pstr, eventType, year, version.toInt, deleteMembersTransform(ua, eventType, memberTransform)).flatMap { _ =>
        compileEventReport(externalId, psaPspId, pstr, eventType, year, version, currentVersion)
      }
      case None => throw new RuntimeException("User answers not available")
    }

  }

  def deleteEvent(externalId: String,
                  psaPspId: String,
                  pstr: String,
                  eventType: EventType,
                  year: Int,
                  version: String,
                  currentVersion: String)
                 (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[Result] = {
    def memberTransform(members: Seq[JsObject]): Seq[JsObject] = {
      members.map(member => member + ("memberStatus", JsString(Deleted().name)))
    }

    def cer = compileEventReport(externalId, psaPspId, pstr, eventType, year, version, currentVersion, deleteEvent = true)

    def processMemberEvents = getUserAnswers(externalId, pstr, eventType, year, version.toInt).flatMap {
      case Some(ua) => saveUserAnswers(externalId, pstr, eventType, year, version.toInt, deleteMembersTransform(ua, eventType, memberTransform)).flatMap { _ =>
        cer
      }
      case None => throw new RuntimeException("User answers not available")
    }

    apiProcessingInfo(eventType, pstr, delete = true) match {
      case Some(APIProcessingInfo(apiType, _, _, _)) =>
        apiType match {
          case ApiType.Api1827 => processMemberEvents
          case ApiType.Api1830 => processMemberEvents
          case _ => cer
        }
      case None => throw new RuntimeException(s"Api for event type $eventType is unavailable")
    }
  }

  private def deleteMembersTransform(ua: JsObject, eventType: EventType, membersTransform: Seq[JsObject] => Seq[JsObject]): JsObject = {
    def membersPath = eventType match {
      case EventType.Event1 => "membersOrEmployers"
      case _ => "members"
    }

    val eventPath = "event" + eventType

    val event = ua.value.getOrElse(eventPath, throw new RuntimeException("Event not available")).as[JsObject].value
    val members = event
      .getOrElse(membersPath, throw new RuntimeException("Members not available"))
      .as[JsArray].value
      .map(_.as[JsObject])

    val newMembers = membersTransform(members.toSeq)

    val newEvent = Json.toJson(event).as[JsObject] + (membersPath, Json.toJson(newMembers))

    ua + (eventPath, newEvent)
  }

  private val api1832Events: List[EventType] = List(Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A, Event22, Event23)
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


  //TODO: old implementation. New implementation doesn't include Event 20A.
  // Below commented out code can be used as a guide for when event20A is implemented
  //  def getVersions(pstr: String, startDate: String)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsArray] = {
  //    val erVersions = eventReportConnector.getVersions(pstr, reportType = "ER", startDate)
  //    val er20AVersions = eventReportConnector.getVersions(pstr, reportType = "ER20A", startDate)
  //    erVersions.flatMap{ g =>er20AVersions.map { h =>
  //      (h.transform(API1537.reads), g.transform(API1537.reads)) match {
  //        case (JsSuccess(j, _), JsSuccess(k, _)) =>
  //          j ++ k
  //        case (JsError(e), _ ) => throw JsResultException(e)
  //        case (_, JsError(e) ) => throw JsResultException(e)
  //      }
  //     }
  //    }
  //  }

  def getVersions(pstr: String, startDate: String)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsArray] = {
    val erVersions = eventReportConnector.getVersions(pstr, reportType = "ER", startDate)

    erVersions.map { jsArray =>
      jsArray.transform(API1537.reads) match {
        case JsSuccess(transformedJsArr, _) => transformedJsArr
        case JsError(e) => throw JsResultException(e)
      }
    }
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

  def submitEventDeclarationReport(pstr: String, psaPspId: String, userAnswersJson: JsValue, version: String)
                                  (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[Result] = {

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

    declarationLockRepository.insertDoubleClickLock(pstr, psaPspId).flatMap { isAvailable =>
      if (isAvailable) {
        for {
          transformed1828Payload <- Future.fromTry(toTry(userAnswersJson.transform(API1828.transformToETMPData)))
          _ <- recoverAndValidatePayload(transformed1828Payload)
        } yield {
          NoContent
        }
      } else {
        Future.successful(BadRequest)
      }
    }
  }


  def submitEvent20ADeclarationReport(pstr: String, psaPspId: String, userAnswersJson: JsValue, version: String)
                                     (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[Result] = {


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

    declarationLockRepository.insertDoubleClickLock(pstr, psaPspId).flatMap { isAvailable =>
      if (isAvailable) {
        for {
          transformed1829Payload <- Future.fromTry(toTry(userAnswersJson.transform(API1829.transformToETMPData)))
          _ <- recoverAndValidatePayload(transformed1829Payload)
        } yield {
          NoContent
        }
      } else {
        Future.successful(BadRequest)
      }
    }
  }
}