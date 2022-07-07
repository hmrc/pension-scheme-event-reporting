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
import models.enumeration.ApiType.{Api1826, Api1827}
import play.api.Logging
import play.api.libs.json.JsValue
import play.api.mvc.Result
import play.api.mvc.Results._
import repositories.EventReportCacheRepository
import uk.gov.hmrc.http.{BadRequestException, HeaderCarrier}
import utils.JSONPayloadSchemaValidator

import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class EventReportService @Inject()(eventReportConnector: EventReportConnector,
                                   eventReportCacheRepository: EventReportCacheRepository,
                                   jsonPayloadSchemaValidator: JSONPayloadSchemaValidator) extends Logging {

  private val createCompiledEventSummaryReportSchemaPath = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  private val compileEventOneReportSchemaPath = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.1.json"

  def compileEventReport(pstr: String, userAnswersJson: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {

    val maybeApi1826 = eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiType" -> Api1826.toString)).map {
      case Some(data) => compileEventReportSummary(pstr, data).map(_ => NoContent)
      case _ => Future.successful(Ok)
    }.flatten

    val maybeApi1827 = eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiType" -> Api1827.toString)).map {
      case Some(data) => compileEventOneReport(pstr, data).map(_ => NoContent)
      case _ => Future.successful(Ok)
    }.flatten

    val seqOfMaybeApiCalls = Future.sequence(Seq(maybeApi1826, maybeApi1827))

    seqOfMaybeApiCalls.map { _ => NoContent }
  }

  private def compileEventReportSummary(pstr: String, data: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    jsonPayloadSchemaValidator.validateJsonPayload(createCompiledEventSummaryReportSchemaPath, data) match {
      case Right(true) =>
        eventReportConnector.compileEventReportSummary(pstr, data).map { response =>
          Ok(response.body)
        }
      case Left(errors) =>
        val allErrorsAsString = "Schema validation errors:-\n" + errors.mkString(",\n")
        throw EventReportValidationFailureException(allErrorsAsString)
      case _ => throw EventReportValidationFailureException("Schema validation failed (returned false)")
    }
  }

  private def compileEventOneReport(pstr: String, data: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    jsonPayloadSchemaValidator.validateJsonPayload(compileEventOneReportSchemaPath, data) match {
      case Right(true) =>
        eventReportConnector.compileEventOneReport(pstr, data).map { response =>
          Ok(response.body)
        }
      case Left(errors) =>
        val allErrorsAsString = "Schema validation errors:-\n" + errors.mkString(",\n")
        throw EventReportValidationFailureException(allErrorsAsString)
      case _ => throw EventReportValidationFailureException("Schema validation failed (returned false)")
    }
  }
}

case class EventReportValidationFailureException(exMessage: String) extends BadRequestException(exMessage)
