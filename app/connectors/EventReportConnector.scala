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
import models.enumeration.ApiType.Api1832
import models.enumeration.EventType
import models.enumeration.EventType.getApiTypeByEventType
import models.{EROverview, ERVersion}
import play.api.Logging
import play.api.http.Status._
import play.api.libs.json._
import uk.gov.hmrc.http.{HttpClient, _}
import utils.HttpResponseHelper

import scala.concurrent.{ExecutionContext, Future}

class EventReportConnector @Inject()(
                                      config: AppConfig,
                                      http: HttpClient,
                                      headerUtils: HeaderUtils
                                    )
  extends HttpErrorFunctions
    with HttpResponseHelper
    with Logging {

  def compileEventReportSummary(pstr: String, data: JsValue)
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

  def compileEventOneReport(pstr: String, data: JsValue)
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

  def compileMemberEventReport(pstr: String, data: JsValue)
                              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val compileMemberEventReportUrl = config.compileMemberEventReportUrl.format(pstr)
    logger.debug("Compile Member Event Report- URL:" + compileMemberEventReportUrl)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)
    http.POST[JsValue, HttpResponse](compileMemberEventReportUrl, data)(implicitly, implicitly, hc, implicitly) map {
      response =>
        response.status match {
          case OK => response
          case _ => handleErrorResponse("POST", compileMemberEventReportUrl)(response)
        }
    }
  }

  def submitEvent20ADeclarationReport(pstr: String, data: JsValue)
                                     (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val submitEvent20ADeclarationReportUrl = config.submitEvent20ADeclarationReportUrl.format(pstr)
    logger.debug("Submit Event 20A Report - URL:" + submitEvent20ADeclarationReportUrl)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)
    http.POST[JsValue, HttpResponse](submitEvent20ADeclarationReportUrl, data)(implicitly, implicitly, hc, implicitly) map {
      response =>
        response.status match {
          case OK => response
          case _ => handleErrorResponse("POST", submitEvent20ADeclarationReportUrl)(response)
        }
    }
  }

  //scalastyle:off cyclomatic.complexity
  def getOverview(pstr: String, reportType: String, startDate: String, endDate: String)
                 (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Seq[EROverview]] = {

    val getErOverviewUrl: String = config.overviewUrl.format(pstr, reportType, startDate, endDate)

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

  def getEvent(pstr: String, startDate: String, version: String, eventType: EventType)
              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsValue] = {
    getApiTypeByEventType(eventType) match {
      case Some(apiType) =>
        val apiToCall = apiType.toString
        val apiUrl: String = s"${config.getApiUrlByApiNum(apiToCall).format(pstr)}"

        val eventTypeParam = if (apiType == Api1832) Seq("eventType" -> s"Event${eventType.toString}") else Seq.empty
        val fullHeaders = integrationFrameworkHeader ++
          eventTypeParam ++
          Seq(
            "reportStartDate" -> startDate,
            "reportVersionNumber" -> version
          )

        logger.debug(s"Get $apiToCall (IF) called - URL: $apiUrl with headers: $fullHeaders")

        implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = fullHeaders: _*)
        http.GET[HttpResponse](apiUrl)(implicitly, hc, implicitly).map { response =>
          response.status match {
            case OK => response.json
            case _ => handleErrorResponse("GET", apiUrl)(response)
          }
        }
      case None => throw new BadRequestException(s"No API is configured to handle getting this eventType: $eventType")
    }
  }

  def submitEventDeclarationReport(pstr: String, data: JsValue)(implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] = {
    val submitEventDeclarationReportUrl = config.submitEventDeclarationReportUrl.format(pstr)
    logger.debug("Submit Event Declaration Report called URL:" + submitEventDeclarationReportUrl)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)
    http.POST[JsValue, HttpResponse](submitEventDeclarationReportUrl, data)(implicitly, implicitly, hc, implicitly) map {
      response =>
        response.status match {
          case OK => response
          case _ => handleErrorResponse("POST", submitEventDeclarationReportUrl)(response)
        }
    }
  }

  def getVersions(pstr: String, reportType: String, startDate: String)
                 (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Seq[ERVersion]] = {

    val versionUrl: String = config.versionUrl.format(pstr, reportType, startDate)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = desHeader: _*)

    http.GET[HttpResponse](versionUrl)(implicitly, hc, implicitly).map {
      response =>
        response.status match {
          case OK =>
            Json.parse(response.body).validate[Seq[ERVersion]](Reads.seq(ERVersion.rds)) match {
              case JsSuccess(versions, _) => versions
              case JsError(errors) => throw JsResultException(errors)
            }
          case _ =>
            handleErrorResponse("GET", versionUrl)(response)
        }
    }
  }

  private def integrationFrameworkHeader: Seq[(String, String)] = {
    Seq("Environment" -> config.integrationFrameworkEnvironment,
      "Authorization" -> config.integrationFrameworkAuthorization,
      "Content-Type" -> "application/json", "CorrelationId" -> headerUtils.getCorrelationId)
  }

  private def desHeader: Seq[(String, String)] = {
    Seq(
      "Environment" -> config.desEnvironment,
      "Authorization" -> config.authorization,
      "Content-Type" -> "application/json",
      "CorrelationId" -> headerUtils.getCorrelationId
    )
  }
}
