/*
 * Copyright 2024 HM Revenue & Customs
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
import models.EROverview
import models.enumeration.ApiType.Api1834
import models.enumeration.EventType.getApiTypeByEventType
import models.enumeration.{ApiType, EventType}
import play.api.Logging
import play.api.http.Status._
import play.api.libs.json._
import play.api.mvc.RequestHeader
import services.PostToAPIAuditService
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.client.HttpClientV2
import utils.HttpResponseHelper

import scala.concurrent.{ExecutionContext, Future}

class EventReportConnector @Inject()(
                                      config: AppConfig,
                                      httpV2Client: HttpClientV2,
                                      headerUtils: HeaderUtils,
                                      postToAPIAuditService: PostToAPIAuditService
                                    )
  extends HttpErrorFunctions
    with HttpResponseHelper
    with Logging {

  private def debugLogs(title:String, url: String, headers: Seq[(String, String)], data: => JsValue): Unit = {
    logger.debug(
      s"""$title:
         |URL: $url
         |Headers:
         |${Json.prettyPrint(Json.toJson(headers))}
         |Data:
         |${Json.prettyPrint(data)}
         |""".stripMargin)
  }

  //scalastyle:off cyclomatic.complexity
  def getOverview(pstr: String, reportType: String, startDate: String, endDate: String)
                 (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Seq[EROverview]] = {

    val getErOverviewUrl: String = config.overviewUrl.format(pstr, reportType, startDate, endDate)

    logger.debug("Get overview (IF) called - URL:" + getErOverviewUrl)

    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)

    httpV2Client.get(url"$getErOverviewUrl")(hc).execute[HttpResponse].map { response =>
      response.status match {
        case OK =>
          Json.parse(response.body).validate[Seq[EROverview]](Reads.seq(EROverview.rds)) match {
            case JsSuccess(data, _) =>
              debugLogs("get overview", getErOverviewUrl, hc.extraHeaders, Json.parse(response.body))
              data
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


  def getEvent(pstr: String, startDate: String, version: String, eventType: Option[EventType])
              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Option[JsObject]] = {
    val headers = integrationFrameworkHeader ++
      Seq(
        "reportStartDate" -> startDate,
        "reportVersionNumber" -> ("00" + version).takeRight(3)
      )

    def getForApi(headers: Seq[(String, String)], pstr: String, api: ApiType, eventType: Option[EventType])
                 (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Option[JsObject]] = {

      val etAsString = eventType.getOrElse("none")

      val apiUrl: String = s"${config.getApiUrlByApiNum(api.toString).format(pstr)}"
      implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = headers: _*)
      val versionAsString = ("00" + version).takeRight(3)
      val logMessage =
        s"Get ${api.toString} (IF) called (URL $apiUrl). Event type: $etAsString reportStartDate: $startDate and reportVersionNumber: $versionAsString"
      logger.info(logMessage)
      httpV2Client
        .get(url"$apiUrl")(hc)
        .transform(_.withRequestTimeout(config.ifsTimeout))
        .execute[HttpResponse].map { response =>
        response.status match {
          case OK =>
            debugLogs("get event API " + api.toString, apiUrl, hc.extraHeaders, response.json)
            Some(response.json.as[JsObject])
          case NOT_FOUND | UNPROCESSABLE_ENTITY =>
            logger.warn(s"$logMessage and returned ${response.status} with message ${response.body}")
            None
          case _ => handleErrorResponse("GET", apiUrl)(response)
        }
      }
    }

    eventType match {
      case Some(et) =>
        getApiTypeByEventType(et) match {
          case Some(api) =>
            val headersWithEventType: Seq[(String, String)] = {
              api match {
                case apiType if apiType == ApiType.Api1832 || apiType == ApiType.Api1834 => headers ++ Seq("eventType" -> s"Event${et.toString}")
                case _ => headers
              }
            }
            getForApi(headersWithEventType, pstr, api, eventType)
          case None =>
            Future.successful(None)
        }
      case _ => getForApi(headers, pstr, Api1834, eventType)
    }
  }




  def compileEventReportSummary(psaPspId: String, pstr: String, data: JsValue, reportVersion: String)
                               (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] = {
    val createCompileEventReportSummaryUrl = config.createCompileEventReportSummaryUrl.format(pstr)
    logger.debug("Compile Event Report Summary called - URL:" + createCompileEventReportSummaryUrl)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)
    httpV2Client.post(url"$createCompileEventReportSummaryUrl")(hc).withBody(data).execute[HttpResponse].map {
      response =>
        response.status match {
          case OK =>
            debugLogs("compile event report summary ", createCompileEventReportSummaryUrl, hc.extraHeaders, data)
            response
          case _ => handleErrorResponse("POST", createCompileEventReportSummaryUrl)(response)
        }
    } andThen postToAPIAuditService.sendCompileEventDeclarationAuditEvent(psaPspId, pstr, data, reportVersion)
  }

  def compileEventOneReport(psaPspId: String, pstr: String, data: JsValue, reportVersion: String)
                           (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] = {
    val compileEvent1ReportUrl = config.compileEvent1ReportUrl.format(pstr)
    logger.debug("Compile Event Report One - URL:" + compileEvent1ReportUrl)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)
    httpV2Client.post(url"$compileEvent1ReportUrl")(hc).withBody(data).execute[HttpResponse].map {
      response =>
        response.status match {
          case OK =>
            debugLogs("compile event 1 API 1827", compileEvent1ReportUrl, hc.extraHeaders, data)
            response
          case _ => handleErrorResponse("POST", compileEvent1ReportUrl)(response)
        }
    } andThen postToAPIAuditService.sendCompileEventDeclarationAuditEvent(psaPspId, pstr, data, reportVersion)
  }

  def compileMemberEventReport(psaPspId: String, pstr: String, data: JsValue, reportVersion: String)
                              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] = {
    val compileMemberEventReportUrl = config.compileMemberEventReportUrl.format(pstr)
    logger.debug("Compile Member Event Report- URL:" + compileMemberEventReportUrl)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)
    httpV2Client.post(url"$compileMemberEventReportUrl")(hc).withBody(data).execute[HttpResponse].map {
      response =>
        response.status match {
          case OK =>
            debugLogs("compile Member Event API 1830", compileMemberEventReportUrl, hc.extraHeaders, data)
            response
          case _ => handleErrorResponse("POST", compileMemberEventReportUrl)(response)
        }
    } andThen postToAPIAuditService.sendCompileEventDeclarationAuditEvent(psaPspId, pstr, data, reportVersion)
  }

  def submitEventDeclarationReport(pstr: String, data: JsValue, reportVersion: String)(implicit headerCarrier: HeaderCarrier,
                                                                                       ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] = {
    val submitEventDeclarationReportUrl = config.submitEventDeclarationReportUrl.format(pstr)
    logger.debug("Submit Event Declaration Report called URL:" + submitEventDeclarationReportUrl)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)
    httpV2Client.post(url"$submitEventDeclarationReportUrl")(hc).withBody(data).execute[HttpResponse].map {
      response =>
        response.status match {
          case OK =>
            debugLogs("submit event declaration report API 1828", submitEventDeclarationReportUrl, hc.extraHeaders, data)
            response
          case _ => handleErrorResponse("POST", submitEventDeclarationReportUrl)(response)
        }
    } andThen postToAPIAuditService.sendSubmitEventDeclarationAuditEvent(pstr, data, reportVersion, None)
  }

  def submitEvent20ADeclarationReport(pstr: String, data: JsValue, reportVersion: String)
                                     (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] = {
    val submitEvent20ADeclarationReportUrl = config.submitEvent20ADeclarationReportUrl.format(pstr)
    logger.debug("Submit Event 20A Report - URL:" + submitEvent20ADeclarationReportUrl)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)
    httpV2Client.post(url"$submitEvent20ADeclarationReportUrl")(hc).withBody(data).execute[HttpResponse].map {
      response =>
        response.status match {
          case OK =>
            debugLogs("submit event declaration report Event20A API 1829", submitEvent20ADeclarationReportUrl, hc.extraHeaders, data)
            response
          case _ => handleErrorResponse("POST", submitEvent20ADeclarationReportUrl)(response)
        }
    } andThen postToAPIAuditService.sendSubmitEventDeclarationAuditEvent(pstr, data, reportVersion, Some(EventType.Event20A))
  }

  def getVersions(pstr: String, reportType: String, startDate: String)
                 (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsArray] = {

    val versionUrl: String = config.versionUrl.format(pstr, reportType, startDate)
    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers = integrationFrameworkHeader: _*)

    httpV2Client.get(url"$versionUrl")(hc).execute[HttpResponse].map {
      response =>
        response.status match {
          case OK =>
            debugLogs("get versions", versionUrl, hc.extraHeaders, Json.obj())
            response.json.as[JsArray]
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
}
