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

import com.google.inject.{ImplementedBy, Inject}
import config.AppConfig
import models.EROverview
import play.api.Logging
import play.api.http.Status._
import play.api.libs.json._
import uk.gov.hmrc.http.{HttpClient, _}
import utils.HttpResponseHelper

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[EventReportConnectorImpl])
trait EventReportConnector {

  def compileEventReportSummary(pstr: String, data: JsValue)
                               (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse]

  def compileEventOneReport(pstr: String, data: JsValue)
                           (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse]

  def getErOverview(pstr: String, startDate: String, endDate: String)
                   (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Seq[EROverview]]
  //scalastyle:off
  def getEr20AOverview(pstr: String, startDate: String, endDate: String)
                      (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Seq[EROverview]]

}

class EventReportConnectorImpl @Inject()(
                                          config: AppConfig,
                                          http: HttpClient,
                                          headerUtils: HeaderUtils
                                        )
  extends EventReportConnector
    with HttpErrorFunctions
    with HttpResponseHelper
    with Logging {



  override def compileEventReportSummary(pstr: String, data: JsValue)
                                        (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val createCompileEventReportSummaryUrl = config.createCompileEventReportSummaryUrl.format(pstr)
    logger.debug("Compile Event Report Summary called - URL:" + createCompileEventReportSummaryUrl)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)
    http.POST[JsValue, HttpResponse](createCompileEventReportSummaryUrl, data)(implicitly, implicitly, hc, implicitly) map {
      response =>
        response.status match {
          case OK => response
          case _ => handleErrorResponse("POST", createCompileEventReportSummaryUrl)(response)
        }
    }
  }

  override def compileEventOneReport(pstr: String, data: JsValue)
                                    (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val compileEvent1ReportUrl = config.compileEvent1ReportUrl.format(pstr)
    logger.debug("Compile Event Report One - URL:" + compileEvent1ReportUrl)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)
    http.POST[JsValue, HttpResponse](compileEvent1ReportUrl, data)(implicitly, implicitly, hc, implicitly) map {
      response =>
        response.status match {
          case OK => response
          case _ => handleErrorResponse("POST", compileEvent1ReportUrl)(response)
        }
    }
  }

  //scalastyle:off cyclomatic.complexity
  override def getErOverview(pstr: String, startDate: String, endDate: String)
                   (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Seq[EROverview]] = {

    val getErOverviewUrl: String = config.getErOverviewUrl.format(pstr, startDate, endDate)

    logger.debug("Get overview (IF) called - URL:" + getErOverviewUrl)

    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)

    http.GET[HttpResponse](getErOverviewUrl)(implicitly, hc, implicitly).map { response =>
      response.status match {
        case OK =>
          Json.parse(response.body).validate[Seq[EROverview]](Reads.seq(EROverview.rds)) match {
            case JsSuccess(data, _) => data
            case JsError(errors) => throw JsResultException(errors)
          }
        case NOT_FOUND =>
          val singleError = (Json.parse(response.body) \ "code").asOpt[String]
          val multipleError = (Json.parse(response.body) \ "failures").asOpt[JsArray]
          (singleError, multipleError) match {
            case (Some(err), _) if err.equals("NO_REPORT_FOUND") =>
              logger.info("The remote endpoint has indicated No Scheme report was found for the given period.")
              Seq.empty[EROverview]
            case (_, Some(seqErr)) =>
              val isAnyNoReportFound = seqErr.value.exists(jsValue => (jsValue \ "code").asOpt[String].contains("NO_REPORT_FOUND"))
              if (isAnyNoReportFound) {
                logger.info("The remote endpoint has indicated No Schema report was found for the given period.")
                Seq.empty[EROverview]
              } else {
                handleErrorResponse("GET", getErOverviewUrl)(response)
              }
            case _ => handleErrorResponse("GET", getErOverviewUrl)(response)
          }
        case _ => handleErrorResponse("GET", getErOverviewUrl)(response)
      }
    }
  }

  //scalastyle:off cyclomatic.complexity
  //scalastyle:off method.name
  override def getEr20AOverview(pstr: String, startDate: String, endDate: String)
                      (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Seq[EROverview]] = {

    val getEr20aOverviewUrl: String = config.getEr20AOverviewUrl.format(pstr, startDate, endDate)

    logger.warn("Get overview (IF) called - URL:" + getEr20aOverviewUrl)

    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)

    http.GET[HttpResponse](getEr20aOverviewUrl)(implicitly, hc, implicitly).map { response =>
      response.status match {
        case OK =>
          Json.parse(response.body).validate[Seq[EROverview]](Reads.seq(EROverview.rds)) match {
            case JsSuccess(data, _) => data
            case JsError(errors) => throw JsResultException(errors)
          }
        case NOT_FOUND =>
          val singleError = (Json.parse(response.body) \ "code").asOpt[String]
          val multipleError = (Json.parse(response.body) \ "failures").asOpt[JsArray]
          (singleError, multipleError) match {
            case (Some(err), _) if err.equals("NO_REPORT_FOUND") =>
              logger.info("The remote endpoint has indicated No Scheme report was found for the given period.")
              Seq.empty[EROverview]
            case (_, Some(seqErr)) =>
              val isAnyNoReportFound = seqErr.value.exists(jsValue => (jsValue \ "code").asOpt[String].contains("NO_REPORT_FOUND"))
              if (isAnyNoReportFound) {
                logger.info("The remote endpoint has indicated No Schema report was found for the given period.")
                Seq.empty[EROverview]
              } else {
                handleErrorResponse("GET", getEr20aOverviewUrl)(response)
              }
            case _ => handleErrorResponse("GET", getEr20aOverviewUrl)(response)
          }
        case _ => handleErrorResponse("GET", getEr20aOverviewUrl)(response)
      }
    }
  }

  private def integrationFrameworkHeader: Seq[(String, String)] = {
    Seq("Environment" -> config.integrationframeworkEnvironment,
      "Authorization" -> config.integrationframeworkAuthorization,
      "Content-Type" -> "application/json", "CorrelationId" -> headerUtils.getCorrelationId)
  }

}
