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
import play.api.libs.json._
import play.api.mvc.Result
import play.api.mvc.Results._
import repositories.{EventReportCacheRepository, OverviewCacheRepository}
import transformations.ETMPToFrontEnd.EventSummary
import uk.gov.hmrc.http.HeaderCarrier
import utils.JSONSchemaValidator

import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class EventReportService @Inject()(eventReportConnector: EventReportConnector,
                                   eventReportCacheRepository: EventReportCacheRepository,
                                   jsonPayloadSchemaValidator: JSONSchemaValidator,
                                   overviewCacheRepository: OverviewCacheRepository
                                  ) {

  private val createCompiledEventSummaryReportSchemaPath = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  private val compileEventOneReportSchemaPath = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.1.json"
  private val compileMemberEventReportSchemaPath = "/resources.schemas/api-1830-create-compiled-member-event-report-request-schema-v1.0.4.json"


  def compileEventReport(pstr: String, userAnswersJson: JsValue)
                        (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {

    val maybeApi1826 = eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1826.toString)).map {
      case Some(data) => compileEventReportSummary(pstr, data).map(_ => NoContent)
      case _ => Future.successful(Ok)
    }.flatten

    val maybeApi1827 = eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1827.toString)).map {
      case Some(data) => compileEventOneReport(pstr, data).map(_ => NoContent)
      case _ => Future.successful(Ok)
    }.flatten

    val maybeApi1830 = eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1830.toString)).map {
      case Some(data) => compileMemberEventReport(pstr, data).map(_ => NoContent)
      case _ => Future.successful(Ok)
    }.flatten

    val seqOfMaybeApiCalls = Future.sequence(Seq(maybeApi1826, maybeApi1827, maybeApi1830))

    seqOfMaybeApiCalls.map { _ => NoContent }
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

  private def compileEventReportSummary(pstr: String, data: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    for {
      _ <- Future.fromTry(jsonPayloadSchemaValidator.validatePayload(data, createCompiledEventSummaryReportSchemaPath, "compileEventReportSummary"))
      response <- eventReportConnector.compileEventReportSummary(pstr, data)
    } yield Ok(response.body)

  private def compileEventOneReport(pstr: String, data: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    for {
      //TODO: PODS-7714 write code to transform json and call it from here
      _ <- Future.fromTry(jsonPayloadSchemaValidator.validatePayload(data, compileEventOneReportSchemaPath, "compileEventOneReport"))
      response <- eventReportConnector.compileEventOneReport(pstr, data)
    } yield Ok(response.body)

  private def compileMemberEventReport(pstr: String, data: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] =
    for {
      _ <- Future.fromTry(jsonPayloadSchemaValidator.validatePayload(data, compileMemberEventReportSchemaPath, "compileMemberEventReport"))
      response <- eventReportConnector.compileMemberEventReport(pstr, data)
    } yield Ok(response.body)
}
