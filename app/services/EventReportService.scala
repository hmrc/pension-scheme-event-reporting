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
import models.enumeration.EventType
import play.api.http.Status.NOT_FOUND
import play.api.libs.json.JsResult.toTry
import play.api.libs.json._
import play.api.mvc.Result
import play.api.mvc.Results._
import repositories.{EventReportCacheRepository, OverviewCacheRepository}
import transformations.ETMPToFrontEnd.EventSummary
import transformations.UserAnswersToETMP.{API1826, API1827}
import uk.gov.hmrc.http.HeaderCarrier
import utils.JSONSchemaValidator

import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class EventReportService @Inject()(eventReportConnector: EventReportConnector,
                                   eventReportCacheRepository: EventReportCacheRepository,
                                   jsonPayloadSchemaValidator: JSONSchemaValidator,
                                   overviewCacheRepository: OverviewCacheRepository
                                  ) {

  private val schemaPath1826 = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  private val schemaPath1827 = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.1.json"
  private val schemaPath1830 = "/resources.schemas/api-1830-create-compiled-member-event-report-request-schema-v1.0.4.json"

  def compileEventReport(pstr: String, eventType: EventType)
                        (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {

    val apiType = EventType.postApiTypeByEventType(eventType)
    val performCompile: (String, JsValue) => Future[Result] =
      apiType match {
        case Api1826 => compile1826 _
        case Api1827 => compile1827 _
        case Api1830 => compile1830 _
        case api => (_, _) =>
          Future.successful(NotFound(s"Compile unimplemented for API type $api (event type $eventType)"))
      }

    eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> apiType.toString)).flatMap {
      case Some(data) =>
        performCompile(pstr, data).map { result =>
          result.header.status match {
            case NOT_FOUND => result
            case _ => NoContent
          }
        }
      case _ => Future.successful(NoContent)
    }
  }

  def getEvent(pstr: String, startDate: String, version: String, eventType: EventType)
              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsValue] = {
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
    eventReportCacheRepository.upsert(pstr, EventType.postApiTypeByEventType(eventType), userAnswersJson)
  }

  def getUserAnswers(pstr: String, eventType: EventType)(implicit ec: ExecutionContext): Future[Option[JsObject]] = {
    eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> EventType.postApiTypeByEventType(eventType).toString))
      .map(_.map(_.as[JsObject]))
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

  private def compile1826(pstr: String, data: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    for {
      transformedData <- Future.fromTry(toTry(data.transform(API1826.transformToETMPData)))
      _ <- Future.fromTry(jsonPayloadSchemaValidator
        .validatePayload(transformedData, schemaPath1826, "API1826"))
      response <- eventReportConnector.compileEventReportSummary(pstr, data)
    } yield {
      Ok(response.body)
    }

  private def compile1827(pstr: String, data: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    for {
      transformedData <- Future.fromTry(toTry(data.transform(API1827.transformToETMPData)))
      _ <- Future.fromTry(jsonPayloadSchemaValidator
        .validatePayload(transformedData, schemaPath1827, "API1827"))
      response <- eventReportConnector.compileEventOneReport(pstr, transformedData)
    } yield {
      Ok(response.body)
    }
  }

  private def compile1830(pstr: String, data: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    for {
      _ <- Future.fromTry(jsonPayloadSchemaValidator
        .validatePayload(data, schemaPath1830, "API1830"))
      response <- eventReportConnector.compileMemberEventReport(pstr, data)
    } yield Ok(response.body)
}
