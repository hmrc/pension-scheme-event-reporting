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
import models.ERVersion
import models.enumeration.ApiType._
import models.enumeration.{ApiType, EventType}
import play.api.Logging
import play.api.http.Status.NOT_IMPLEMENTED
import play.api.libs.json.JsResult.toTry
import play.api.libs.json._
import play.api.mvc.Result
import play.api.mvc.Results._
import repositories.{EventReportCacheRepository, OverviewCacheRepository}
import transformations.ETMPToFrontEnd.EventSummary.{rdsEventTypeNodeOnly, rdsFor1834}
import transformations.UserAnswersToETMP.{API1826, API1827, API1830}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import utils.JSONSchemaValidator

import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class EventReportService @Inject()(eventReportConnector: EventReportConnector,
                                   eventReportCacheRepository: EventReportCacheRepository,
                                   jsonPayloadSchemaValidator: JSONSchemaValidator,
                                   overviewCacheRepository: OverviewCacheRepository
                                  ) extends Logging {
  private final val SchemaPath1826 = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  private final val SchemaPath1827 = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.1.json"
  private final val SchemaPath1830 = "/resources.schemas/api-1830-create-compiled-member-event-report-request-schema-v1.0.4.json"

  private case class APIProcessingInfo(apiType: ApiType,
                                       readsForTransformation: Reads[JsObject],
                                       schemaPath: String,
                                       connectToAPI: (String, JsValue) => Future[HttpResponse]
                                      )

  private def apiProcessingInfo(eventType: EventType, pstr: String)
                               (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Option[APIProcessingInfo] = {
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

  def saveUserAnswers(pstr: String, eventType: EventType, userAnswersJson: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    EventType.postApiTypeByEventType(eventType) match {
      case Some(apiType) => eventReportCacheRepository.upsert(pstr, apiType, userAnswersJson)
      case _ => Future.successful(())
    }
  }

  def saveUserAnswers(pstr: String, userAnswersJson: JsValue)(implicit ec: ExecutionContext): Future[Unit] =
    eventReportCacheRepository.upsert(pstr, userAnswersJson)

  def getUserAnswers(pstr: String, eventType: EventType)(implicit ec: ExecutionContext): Future[Option[JsObject]] =
    EventType.postApiTypeByEventType(eventType) match {
      case Some(apiType) => eventReportCacheRepository.getUserAnswers(pstr, Some(apiType))
      case _ => Future.successful(None)
    }

  def getUserAnswers(pstr: String)(implicit ec: ExecutionContext): Future[Option[JsObject]] =
    eventReportCacheRepository.getUserAnswers(pstr, None)

  def compileEventReport(pstr: String, eventType: EventType)
                        (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    apiProcessingInfo(eventType, pstr) match {
      case Some(APIProcessingInfo(apiType, reads, schemaPath, connectToAPI)) =>
        eventReportCacheRepository.getUserAnswers(pstr, Some(apiType)).flatMap {
          case Some(data) =>
            eventReportCacheRepository.getUserAnswers(pstr, None).flatMap {
              case Some(header) =>
                val fullData = header ++ data
                for {
                  transformedData <- Future.fromTry(toTry(fullData.validate(reads)))
                  _ <- Future.fromTry(jsonPayloadSchemaValidator.validatePayload(transformedData, schemaPath, apiType.toString))
                  response <- connectToAPI(pstr, transformedData)
                } yield {
                  response.status match {
                    case NOT_IMPLEMENTED => BadRequest(s"Not implemented - event type $eventType")
                    case _ =>
                      logger.debug(s"SUCCESSFUL SUBMISSION TO COMPILE API $apiType: $transformedData")
                      NoContent
                  }
                }
              case None => Future.successful(NotFound)
            }
          case _ => Future.successful(NotFound)
        }
      case _ => Future.successful(BadRequest(s"Compile unimplemented for event type $eventType"))
    }
  }

  def getEvent(pstr: String, startDate: String, version: String, eventType: EventType)
              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Option[JsValue]] = {
    eventReportConnector.getEvent(pstr, startDate, version, Some(eventType))
  }

  def getEventSummary(pstr: String, version: String, startDate: String)
                     (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsArray] = {

    val transformedFutures = for {
      /*
        The reads for 1832 below are only checking whether the node for the eventType in question EXISTS.
        It is not parsing the whole file. This is so that we know which event types to display on summary page.
       */
      eventTypeReadPairs <- Map(
        Some(EventType.Event22) -> rdsEventTypeNodeOnly(EventType.Event22),
        Some(EventType.Event23) -> rdsEventTypeNodeOnly(EventType.Event23),
        None -> rdsFor1834
      )
    } yield {
      eventReportConnector.getEvent(pstr, startDate, version, eventTypeReadPairs._1).map { optEtmpJson =>
        optEtmpJson.map { etmpJson =>
          etmpJson.transform(eventTypeReadPairs._2) match {
            case JsSuccess(seqOfEventTypes, _) =>
              seqOfEventTypes
            case JsError(errors) =>
              throw JsResultException(errors)
          }
        }
      }
    }

    Future.sequence(transformedFutures).map { listOfJsArrays =>
      val combinedJsArray = listOfJsArrays.flatten.reduce((jsArrayA, jsArrayB) => jsArrayA ++ jsArrayB)
      JsArray(combinedJsArray.value.sortWith(sortEventTypes))
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

  def submitEventDeclarationReport(pstr: String, userAnswersJson: JsValue)
                                  (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsValue] = {
    eventReportConnector.submitEventDeclarationReport(pstr, userAnswersJson).map(_.json)
  }

  def submitEvent20ADeclarationReport(pstr: String, data: JsValue)
                                     (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsValue] = {
    eventReportConnector.submitEvent20ADeclarationReport(pstr, data).map(_.json)
  }

  private val sortEventTypes: (JsValue, JsValue) => Boolean = (a, b) =>
    (a, b) match {
      case (JsString("0"), _) => false
      case (_, JsString("0")) => true
      case (a: JsString, b: JsString) if EventType.getEventType(a.value).get.order < EventType.getEventType(b.value).get.order => true
      case _ => false
    }
}
