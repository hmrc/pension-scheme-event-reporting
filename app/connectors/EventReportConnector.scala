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

package connectors

import com.google.inject.Inject
import config.AppConfig
import play.api.Logger
import play.api.http.Status._
import play.api.libs.json._
import uk.gov.hmrc.http.{HttpClient, _}
import utils.HttpResponseHelper

import scala.concurrent.{ExecutionContext, Future}

class EventReportConnector @Inject()(
                                      http: HttpClient,
                                      config: AppConfig,
                                      headerUtils: HeaderUtils
                                    )
  extends HttpErrorFunctions
    with HttpResponseHelper {

  private val logger = Logger(classOf[EventReportConnector])

  def compileEventReportSummary(pstr: String, data: JsValue)
                               (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val createCompileEventReportSummaryUrl = config.createCompileEventReportSummaryUrl.format(pstr)
    logger.warn("Compile Event Report Summary called - URL:" + createCompileEventReportSummaryUrl)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)
    http.POST[JsValue, HttpResponse](createCompileEventReportSummaryUrl, data)(implicitly, implicitly, hc, implicitly) map {
      response =>
        response.status match {
          case OK => response
          case _ => handleErrorResponse("POST", createCompileEventReportSummaryUrl)(response)
        }
    }
  }

  private def integrationFrameworkHeader: Seq[(String, String)] = {
    Seq("Environment" -> config.integrationframeworkEnvironment,
      "Authorization" -> config.integrationframeworkAuthorization,
      "Content-Type" -> "application/json", "CorrelationId" -> headerUtils.getCorrelationId)
  }
}
