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
import models.enumeration.EventType.{Event1, Event2, Event20A, Event22, Event23, Event24, Event3, Event4, Event5, Event6, Event7, Event8, Event8A}
import models.enumeration.{ApiType, EventType}
import models.{ERVersion, EventDataIdentifier}
import play.api.Logging
import play.api.http.Status.NOT_IMPLEMENTED
import play.api.libs.json.JsResult.toTry
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc.{RequestHeader, Result}
import repositories.{EventReportCacheRepository, OverviewCacheRepository}
import transformations.ETMPToFrontEnd.{Event20AReport, EventOneReport, MemberEventReport}
import transformations.UserAnswersToETMP._
import uk.gov.hmrc.http.{BadRequestException, ExpectationFailedException, HeaderCarrier, HttpResponse}
import utils.JSONSchemaValidator

import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class EventReportService @Inject()(eventReportConnector: EventReportConnector,
                                   eventReportCacheRepository: EventReportCacheRepository,
                                   jsonPayloadSchemaValidator: JSONSchemaValidator,
                                   overviewCacheRepository: OverviewCacheRepository
                                  ) extends Logging {
  private final val SchemaPath1826 = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  private final val SchemaPath1827 = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.4.json"
  private final val SchemaPath1830 = "/resources.schemas/api-1830-create-compiled-member-event-report-request-schema-v1.0.7.json"
  private final val SchemaPath1828 = "/resources.schemas/api-1828-submit-event-declaration-report-request-schema-v1.0.4.json"
  private final val SchemaPath1829 = "/resources.schemas/api-1829-submit-event20a-declaration-report-request-schema-v1.0.0.json"

  private case class APIProcessingInfo(apiType: ApiType,
                                       readsForTransformation: Reads[JsObject],
                                       schemaPath: String,
                                       connectToAPI: (String, String, JsValue) => Future[HttpResponse]
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

  def saveUserAnswers(pstr: String, eventType: EventType, year: Int, version: Int, userAnswersJson: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    EventType.postApiTypeByEventType(eventType) match {
      case Some(apiType) => eventReportCacheRepository.upsert(pstr, EventDataIdentifier(apiType, year, version), userAnswersJson)
      case _ => Future.successful(())
    }
  }

  def saveUserAnswers(pstr: String, userAnswersJson: JsValue)(implicit ec: ExecutionContext): Future[Unit] =
    eventReportCacheRepository.upsert(pstr, userAnswersJson)

  def removeUserAnswers(pstr: String)(implicit ec: ExecutionContext): Future[Unit] =
    eventReportCacheRepository.removeAllOnSignOut(pstr)

  def getUserAnswers(pstr: String, eventType: EventType, year: Int, version: Int)(implicit ec: ExecutionContext): Future[Option[JsObject]] =
    EventType.postApiTypeByEventType(eventType) match {
      case Some(apiType) => eventReportCacheRepository.getUserAnswers(pstr, Some(EventDataIdentifier(apiType, year, version)))
      case _ => Future.successful(None)
    }

  def getUserAnswers(pstr: String)(implicit ec: ExecutionContext): Future[Option[JsObject]] =
    eventReportCacheRepository.getUserAnswers(pstr, None)

  def compileEventReport(psaPspId: String, pstr: String, eventType: EventType, year: Int, version: Int)
                        (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[Result] = {
    apiProcessingInfo(eventType, pstr) match {
      case Some(APIProcessingInfo(apiType, reads, schemaPath, connectToAPI)) =>
        eventReportCacheRepository.getUserAnswers(pstr, Some(EventDataIdentifier(apiType, year, version))).flatMap {
          case Some(data) =>
            val header = Json.obj(
              "taxYear" -> year.toString
            )
            val fullData = data ++ header
            for {
              transformedData <- Future.fromTry(toTry(fullData.validate(reads)))
              _ <- Future.fromTry(jsonPayloadSchemaValidator.validatePayload(transformedData, schemaPath, apiType.toString))
              response <- connectToAPI(psaPspId, pstr, transformedData)
            } yield {
              response.status match {
                case NOT_IMPLEMENTED => BadRequest(s"Not implemented - event type $eventType")
                case _ =>
                  logger.debug(s"SUCCESSFUL SUBMISSION TO COMPILE API $apiType: $transformedData")
                  NoContent
              }
            }
          case None => Future.successful(NotFound)

          case _ => Future.successful(NotFound)
        }
      case _ => Future.successful(BadRequest(s"Compile unimplemented for event type $eventType"))
    }
  }

  private def validationCheck(data: JsValue, eventType: EventType): Option[JsValue] = {

    val api1832Events: List[EventType] = List(Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A, Event22, Event23, Event24)
    eventType match {
      case Event1 => data.validate(EventOneReport.rds1833Api) match {
        case JsSuccess(transformedData, _) => Some(transformedData)
        case _ => None
      }
      case Event20A => data.validate(Event20AReport.rds1831Api) match {
        case JsSuccess(transformedData, _) => Some(transformedData)
        case _ => None
      }
      case evType1832 if api1832Events.contains(evType1832) =>
        data.validate(MemberEventReport.rds1832Api(evType1832)) match {
          case JsSuccess(transformedData, _) => Some(transformedData)
          case _ => None
        }
      case _ => Some(data)
    }
  }

  def getEvent(pstr: String, startDate: String, version: Int, eventType: EventType)
              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Option[JsValue]] = {
    eventReportConnector.getEvent(pstr, startDate, version, Some(eventType)).flatMap {
      case Some(data) =>
        val jsDataOpt = validationCheck(data, eventType)
        Future.successful(Some(Json.toJson(jsDataOpt)))
      case _ =>
        Future.successful(None)
    }
  }

  def getEventSummary(pstr: String, version: Int, startDate: String)
                     (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsArray] = {

    //TODO: Implement for event 20A. I assume API 1831 will need to be used for this. -Pavel Vjalicin
    eventReportConnector.getEvent(pstr, startDate, version, None).map { etmpJsonOpt =>
      etmpJsonOpt.map { etmpJson =>
        etmpJson.transform(transformations.ETMPToFrontEnd.EventSummary.rdsFor1834) match {
          case JsSuccess(seqOfEventTypes, _) => seqOfEventTypes
          case JsError(errors) => throw JsResultException(errors)
        }
      }.getOrElse(JsArray())
    }
  }

  def getVersions(pstr: String, reportType: String, startDate: String)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Seq[ERVersion]] = {
    eventReportConnector.getVersions(pstr, reportType, startDate)
  }

  def getOverview(pstr: String, reportType: String, startDate: String, endDate: String)
                 (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsValue] = {
    overviewCacheRepository.get(pstr, reportType, startDate, endDate).flatMap {
      case Some(data) => Future.successful(data)
      case _ => eventReportConnector.getOverview(pstr, reportType, startDate, endDate).flatMap {
        data =>
          overviewCacheRepository.upsert(pstr, reportType, startDate, endDate, Json.toJson(data))
            .map { _ => Json.toJson(data) }
      }
    }
  }

  private def validatePayloadAgainstSchema(payload: JsObject, schemaPath: String, eventName: String)
                                          (implicit ec: ExecutionContext): Future[Unit] = {
    Future.fromTry(jsonPayloadSchemaValidator.validatePayload(payload, schemaPath, eventName)).map(_ => (): Unit)
  }

  def submitEventDeclarationReport(pstr: String, userAnswersJson: JsValue)
                                  (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[Unit] = {

    def recoverAndValidatePayload(transformed1828Payload: JsObject): Future[Unit] = {
      val recoveredConnectorCallForAPI1828 = eventReportConnector.submitEventDeclarationReport(pstr, transformed1828Payload).map(_.json.as[JsObject]).recover {
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


  def submitEvent20ADeclarationReport(pstr: String, userAnswersJson: JsValue)
                                     (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[Unit] = {


    def recoverAndValidatePayload(transformed1829Payload: JsObject): Future[Unit] = {

      val recoveredConnectorCallForAPI1829 = eventReportConnector.submitEvent20ADeclarationReport(pstr, transformed1829Payload).map(_.json.as[JsObject]).recover {
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