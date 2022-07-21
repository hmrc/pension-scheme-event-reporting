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
import play.api.Logging
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.mvc.Result
import play.api.mvc.Results._
import repositories.{EventReportCacheRepository, OverviewCacheRepository}
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier, NotFoundException}
import utils.JSONPayloadSchemaValidator

import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class EventReportService @Inject()(eventReportConnector: EventReportConnector,
                                   eventReportCacheRepository: EventReportCacheRepository,
                                   jsonPayloadSchemaValidator: JSONPayloadSchemaValidator,
                                   overviewCacheRepository: OverviewCacheRepository
                                  ) extends Logging {

  private val createCompiledEventSummaryReportSchemaPath = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  private val compileEventOneReportSchemaPath = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.1.json"
  private val compileMemberEventReportSchemaPath = "/resources.schemas/api-1830-create-compiled-member-event-report-request-schema-v1.0.4.json"
  private val submitEvent20ADeclarationReportSchemaPath = "/resources.schemas/api-1829-submit-event20a-declaration-report-request-schema-v1.0.0.json"

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

    val maybeApi1829 = eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1829.toString)).map {
      case Some(data) => submitEvent20ADeclarationReport(pstr, data).map(_ => NoContent)
      case _ => Future.successful(Ok)
    }.flatten

    val maybeApi1830 = eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> Api1830.toString)).map {
      case Some(data) => compileMemberEventReport(pstr, data).map(_ => NoContent)
      case _ => Future.successful(Ok)
    }.flatten

    val seqOfMaybeApiCalls = Future.sequence(Seq(maybeApi1826, maybeApi1827, maybeApi1829, maybeApi1830))

    seqOfMaybeApiCalls.map { _ => NoContent }
  }

  def getEvent(pstr: String, startDate: String, version: String, eventType: EventType)
              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsValue] = {
    EventType.getApiTypeByEventType(eventType) match {
      case Some(Api1831) | Some(Api1832) | Some(Api1833) => eventReportConnector.getEvent(pstr, startDate, version, eventType)
      case _ => Future.failed(new NotFoundException(s"Not Found: ApiType not found for eventType ($eventType)"))
    }
  }

  def saveUserAnswers(pstr: String, eventType: EventType, userAnswersJson: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    EventType.postApiTypeByEventType(eventType) match {
      // TODO: Have discussion on potential for overwriting in Mongo.
      case Some(apiType) => eventReportCacheRepository.upsert(pstr, apiType, userAnswersJson)
      case _ => Future.failed(new NotFoundException(s"Not Found: ApiType not found for eventType ($eventType)"))
    }
  }

  def getUserAnswers(pstr: String, eventType: EventType)(implicit ec: ExecutionContext): Future[Option[JsObject]] = {
    EventType.postApiTypeByEventType(eventType) match {
      case Some(apiType) =>
        eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> apiType.toString))
          .map(_.map(_.as[JsObject]))
      case _ => Future.failed(new NotFoundException(s"Not Found: ApiType not found for eventType ($eventType)"))
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

  private def compileEventReportSummary(pstr: String, data: JsValue)
                                       (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    validatePayload(pstr, data, createCompiledEventSummaryReportSchemaPath, "compileEventReportSummary")(
      eventReportConnector.compileEventReportSummary(pstr, data).map { response =>
        Ok(response.body)
      })
  }

  private def compileEventOneReport(pstr: String, data: JsValue)
                                   (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    validatePayload(pstr, data, compileEventOneReportSchemaPath, "compileEventOneReport")(
      eventReportConnector.compileEventOneReport(pstr, data).map { response =>
        Ok(response.body)
      })
  }

  private def compileMemberEventReport(pstr: String, data: JsValue)
                                      (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    validatePayload(pstr, data, compileMemberEventReportSchemaPath, "compileMemberEventReport")(
      eventReportConnector.compileMemberEventReport(pstr, data).map { response =>
        Ok(response.body)
      })
  }

  private def submitEvent20ADeclarationReport(pstr: String, data: JsValue)
                                             (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    validatePayload(pstr, data, submitEvent20ADeclarationReportSchemaPath, "submitEvent20ADeclarationReport")(
      eventReportConnector.submitEvent20ADeclarationReport(pstr, data).map { response =>
        Ok(response.body)
      })
  }

  private def validatePayload[A](pstr: String, data: JsValue, apiSchemaPath: String, eventName: String)(f: => A): A = {
    jsonPayloadSchemaValidator.validateJsonPayload(apiSchemaPath, data) match {
      case Right(true) =>
        f
      case Left(errors) =>
        val allErrorsAsString = s"Schema validation errors for $eventName:-\n" + errors.mkString(",\n")
        throw EventReportValidationFailureException(allErrorsAsString)
      case _ => throw EventReportValidationFailureException(s"$eventName schema validation failed (returned false)")
    }
  }
}

case class EventReportValidationFailureException(exMessage: String) extends BadRequestException(exMessage)

