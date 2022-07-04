
package services


import com.google.inject.Inject
import connectors.EventReportConnector
import controllers.EventReportValidationFailureException
import models.enumeration.ApiTypes
import play.api.libs.json.JsValue
import play.api.mvc.Result
import play.api.mvc.Results._
import repositories.EventReportCacheRepository
import uk.gov.hmrc.http.HeaderCarrier
import utils.JSONPayloadSchemaValidator

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class EventReportService @Inject()(eventReportConnector: EventReportConnector,
                                   eventReportCacheRepository: EventReportCacheRepository,
                                   jsonPayloadSchemaValidator: JSONPayloadSchemaValidator) {

  private val createCompiledEventSummaryReportSchemaPath = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  // private val compileEventOneReportSchemaPath = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.1.json"

  def compileEventReport(pstr: String, userAnswersJson: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
    // TODO: Hard coded Api
    val maybeDataInCache1826 = eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> ApiTypes.Api1826.toString))
    maybeDataInCache1826.map { data =>
      data.map { apiValue =>
        compileEventReportSummary(pstr, apiValue)
        NoContent
      }.getOrElse(NoContent)
    }

    // TODO: Hard coded Api
    val maybeDataInCache1827 = eventReportCacheRepository.getByKeys(Map("pstr" -> pstr, "apiTypes" -> ApiTypes.Api1827.toString))
    maybeDataInCache1827.map { data =>
      data.map { apiValue =>
        compileEventReportSummary(pstr, apiValue)
        NoContent
      }.getOrElse(NoContent)
    }
  }


  def compileEventReportSummary(pstr: String, data: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Result] = {
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
}
