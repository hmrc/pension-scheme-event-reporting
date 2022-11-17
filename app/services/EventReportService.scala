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

package services


import com.google.inject.{Inject, Singleton}
import connectors.EventReportConnector
import models.ERVersion
import models.enumeration.ApiType._
import models.enumeration.{ApiType, EventType}
import play.api.http.Status.{NOT_FOUND, NOT_IMPLEMENTED}
import play.api.libs.json.JsResult.toTry
import play.api.libs.json._
import play.api.mvc.Result
import play.api.mvc.Results._
import repositories.{EventReportCacheRepository, OverviewCacheRepository}
import transformations.ETMPToFrontEnd.EventSummary
import transformations.UserAnswersToETMP.{API1826, API1827, API1830}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import utils.JSONSchemaValidator

import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class EventReportService @Inject()(eventReportConnector: EventReportConnector,
                                   eventReportCacheRepository: EventReportCacheRepository,
                                   jsonPayloadSchemaValidator: JSONSchemaValidator,
                                   overviewCacheRepository: OverviewCacheRepository
                                  ) {
  private final val SchemaPath1826 = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  private final val SchemaPath1827 = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.1.json"
  private final val SchemaPath1830 = "/resources.schemas/api-1830-create-compiled-member-event-report-request-schema-v1.0.4.json"

  private case class APIProcessingInfo(apiType: ApiType,
                                       readsForTransformation: Reads[Option[JsObject]],
                                       schemaPath: String,
                                       connectToAPI: (String, Option[JsValue]) => Future[HttpResponse]
                                      )

  private def connectToAPI(connectFunctionForAPI: (String, JsValue) => Future[HttpResponse]): (String, Option[JsValue]) => Future[HttpResponse] =
    (pstr, optionJsValue) => optionJsValue match {
      case None => Future.successful(HttpResponse(NOT_FOUND, "No data to submit"))
      case Some(jsValue) => connectFunctionForAPI(pstr, jsValue)
    }

  private def apiProcessingInfo(eventType: EventType)
                               (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Option[APIProcessingInfo] = {
    EventType.postApiTypeByEventType(eventType) flatMap {
      case Api1826 =>
        Some(APIProcessingInfo(Api1826, API1826.transformToETMPData, SchemaPath1826, connectToAPI(eventReportConnector.compileEventReportSummary _)))
      case Api1827 =>
        Some(APIProcessingInfo(Api1827, API1827.transformToETMPData, SchemaPath1827, connectToAPI(eventReportConnector.compileEventOneReport _)))
      case Api1830 =>
        Some(APIProcessingInfo(Api1830, API1830.transformToETMPData, SchemaPath1830, connectToAPI(eventReportConnector.compileMemberEventReport _)))
      case _ => None
    }
  }

  private def validatePayloadAgainstSchema(optionTransformedData: Option[JsObject], schemaPath: String, apiType: String): Future[Unit] =
    optionTransformedData match {
      case None => Future.successful(())
      case Some(transformedData) => Future.fromTry(jsonPayloadSchemaValidator.validatePayload(transformedData, schemaPath, apiType))
    }

  def compileEventReport(pstr: String, eventType: EventType)
                        (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    apiProcessingInfo(eventType) match {
      case Some(APIProcessingInfo(apiType, reads, schemaPath, connectToAPI)) =>
        eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> apiType.toString)).flatMap {
          case Some(data) =>
            for {
              optionTransformedData <- Future.fromTry(toTry(data.validate(reads)))
              _ <- validatePayloadAgainstSchema(optionTransformedData, schemaPath, apiType.toString)
              response <- connectToAPI(pstr, optionTransformedData)
            } yield {
              response.status match {
                case NOT_IMPLEMENTED => BadRequest(s"Not implemented - event type $eventType")
                case NOT_FOUND => NotFound(s"Not found - event type $eventType")
                case _ => println("\n\n\n transformedData " + optionTransformedData)
                  NoContent
              }
            }
          case _ => Future.successful(NotFound)
        }
      case _ => Future.successful(BadRequest(s"Compile unimplemented for event type $eventType"))
    }
  }

  def getEvent(pstr: String, startDate: String, version: String, eventType: EventType)
              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Option[JsValue]] = {
    eventReportConnector.getEvent(pstr, startDate, version, eventType)
  }

  def getEventSummary(pstr: String, version: String, startDate: String)
                     (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsArray] = {
    for {
      etmpJson <- eventReportConnector.getEventSummary(pstr, startDate, version)
    } yield {
      etmpJson.transform(EventSummary.rds) match {
        case JsSuccess(seqOfEventTypes, _) =>
          seqOfEventTypes
        case JsError(errors) =>
          throw JsResultException(errors)
      }
    }
  }

  def saveUserAnswers(pstr: String, eventType: EventType, userAnswersJson: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    EventType.postApiTypeByEventType(eventType) match {
      case Some(apiType) => eventReportCacheRepository.upsert(pstr, apiType, userAnswersJson)
      case _ => Future.successful(())
    }
  }

  def getUserAnswers(pstr: String, eventType: EventType)(implicit ec: ExecutionContext): Future[Option[JsObject]] =
    EventType.postApiTypeByEventType(eventType) match {
      case Some(apiType) =>
        eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> apiType.toString))
          .map(_.map(_.as[JsObject]))
      case _ => Future.successful(None)
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
}
