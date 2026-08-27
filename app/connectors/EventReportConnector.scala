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
import models.enumeration.ApiType.*
import models.enumeration.EventType.getApiTypeByEventType
import models.enumeration.{ApiType, EventType}
import play.api.Logging
import play.api.http.Status.*
import play.api.libs.json.*
import play.api.libs.ws.WSBodyWritables.writeableOf_JsValue
import play.api.mvc.RequestHeader
import services.PostToAPIAuditService
import uk.gov.hmrc.http.*
import uk.gov.hmrc.http.client.HttpClientV2
import utils.HttpResponseHelper

import scala.concurrent.{ExecutionContext, Future}

class EventReportConnector @Inject()(
                                      config: AppConfig,
                                      httpV2Client: HttpClientV2,
                                      headerUtils: HeaderUtils,
                                      postToAPIAuditService: PostToAPIAuditService
                                    )
  extends HttpResponseHelper
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

    val url: String = config.overviewUrl.format(pstr, reportType, startDate, endDate)

    logger.debug(s"Get overview (IF) called - URL:$url")

    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(integrationFrameworkHeader*)

    httpV2Client
      .get(url"$url")(hc)
      .transform(_.withRequestTimeout(config.ifsTimeout))
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            Json.parse(response.body).validate[Seq[EROverview]](Reads.seq(EROverview.rds)) match {
              case JsSuccess(data, _) =>
                debugLogs("get overview", url, hc.extraHeaders, Json.parse(response.body))
                data
              case JsError(errors) =>
                throw JsResultException(errors)
            }
          case NOT_FOUND =>
            (
              (Json.parse(response.body) \ "code").asOpt[String],
              (Json.parse(response.body) \ "failures").asOpt[JsArray]
            ) match {
              case (Some(err), _) if err.equals("NO_REPORT_FOUND") =>
                logger.info("The remote endpoint has indicated No Scheme report was found for the given period.")
                Seq.empty[EROverview]
              case (_, Some(seqErr)) if seqErr.value.exists(jsValue => (jsValue \ "code").asOpt[String].contains("NO_REPORT_FOUND")) =>
                logger.info("The remote endpoint has indicated No Scheme report was found for the given period.")
                Seq.empty[EROverview]
              case _ =>
                handleErrorResponse("GET", url)(response)
            }
          case _ =>
            handleErrorResponse("GET", url)(response)
        }
      }
  }

  private def getForApi(headers: Seq[(String, String)], pstr: String, api: ApiType, eventType: Option[EventType], version: String, startDate: String)
                       (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Option[JsObject]] = {

    val url: String = config.apiUrl(api).format(pstr)

    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(headers *)

    val logMessage =
      s"Get ${api.toString} (IF) called (URL $url). " +
        s"Event type: ${eventType.getOrElse(EventType.EventTypeNone)} " +
        s"reportStartDate: $startDate and reportVersionNumber: $version"

    logger.info(logMessage)

    httpV2Client
      .get(url"$url")(hc)
      .transform(_.withRequestTimeout(config.ifsTimeout))
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            debugLogs("get event API " + api.toString, url, hc.extraHeaders, response.json)
            Some(response.json.as[JsObject])
          case NOT_FOUND | UNPROCESSABLE_ENTITY =>
            logger.warn(s"$logMessage and returned ${response.status} with message ${response.body}")
            None
          case _ =>
            handleErrorResponse("GET", url)(response)
        }
      }
  }

  def getEvent(pstr: String, startDate: String, version: String, eventType: Option[EventType])
              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Option[JsObject]] = {
    val formattedVersion: String =
      s"00$version".takeRight(3)

    val headers = integrationFrameworkHeader ++
      Seq(
        "reportStartDate"     -> startDate,
        "reportVersionNumber" -> formattedVersion
      )

    eventType match {
      case Some(et) =>
        getApiTypeByEventType(et) match {
          case Some(api) =>
            val headersWithEventType: Seq[(String, String)] =
              api match {
                case apiType if apiType == Api1832 || apiType == Api1834 =>
                  headers ++ Seq("eventType" -> s"Event${et.toString}")
                case _ =>
                  headers
              }
            getForApi(headersWithEventType, pstr, api, eventType, formattedVersion, startDate)
          case None =>
            Future.successful(None)
        }
      case _ =>
        getForApi(headers, pstr, Api1834, eventType, formattedVersion, startDate)
    }
  }

  def compileEventReportSummary(psaPspId: String, pstr: String, data: JsValue, reportVersion: String)
                               (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] = {

    val url: String = config.apiUrl(Api1826).format(pstr)

    logger.debug(s"Compile Event Report Summary called - URL:$url")

    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(integrationFrameworkHeader*)

    httpV2Client
      .post(url"$url")(hc)
      .withBody(data)
      .transform(_.withRequestTimeout(config.ifsTimeout))
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            debugLogs("compile event report summary ", url, hc.extraHeaders, data)
            response
          case _ =>
            handleErrorResponse("POST", url)(response)
        }
      }
      .andThen {
        postToAPIAuditService.sendCompileEventDeclarationAuditEvent(psaPspId, pstr, data, reportVersion)
      }
  }

  def compileEventOneReport(psaPspId: String, pstr: String, data: JsValue, reportVersion: String)
                           (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] = {

    val url: String = config.apiUrl(Api1827).format(pstr)

    logger.debug(s"Compile Event Report One - URL:$url")

    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(integrationFrameworkHeader*)

    httpV2Client
      .post(url"$url")(hc)
      .withBody(data)
      .transform(_.withRequestTimeout(config.ifsTimeout))
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            debugLogs("compile event 1 API 1827", url, hc.extraHeaders, data)
            response
          case _ =>
            handleErrorResponse("POST", url)(response)
        }
      }
      .andThen {
        postToAPIAuditService.sendCompileEventDeclarationAuditEvent(psaPspId, pstr, data, reportVersion)
      }
  }

  def compileMemberEventReport(psaPspId: String, pstr: String, data: JsValue, reportVersion: String)
                              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] = {

    val url: String = config.apiUrl(Api1830).format(pstr)

    logger.debug(s"Compile Member Event Report- URL:$url")

    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(integrationFrameworkHeader*)

    httpV2Client
      .post(url"$url")(hc)
      .withBody(data)
      .transform(_.withRequestTimeout(config.ifsTimeout))
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            debugLogs("compile Member Event API 1830", url, hc.extraHeaders, data)
            response
          case _ =>
            handleErrorResponse("POST", url)(response)
        }
      }
      .andThen {
        postToAPIAuditService.sendCompileEventDeclarationAuditEvent(psaPspId, pstr, data, reportVersion)
      }
  }

  def submitEventDeclarationReport(pstr: String, data: JsValue, reportVersion: String)
                                  (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] = {

    val url: String = config.apiUrl(Api1828).format(pstr)

    logger.debug(s"Submit Event Declaration Report called URL:$url")

    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(integrationFrameworkHeader*)

    httpV2Client
      .post(url"$url")(hc)
      .withBody(data)
      .transform(_.withRequestTimeout(config.ifsTimeout))
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            debugLogs("submit event declaration report API 1828", url, hc.extraHeaders, data)
            response
          case _ =>
            handleErrorResponse("POST", url)(response)
        }
      }
      .andThen {
        postToAPIAuditService.sendSubmitEventDeclarationAuditEvent(pstr, data, reportVersion, None)
      }
  }

  def submitEvent20ADeclarationReport(pstr: String, data: JsValue, reportVersion: String)
                                     (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] = {

    val url: String = config.apiUrl(Api1829).format(pstr)

    logger.debug(s"Submit Event 20A Report - URL:$url")

    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(integrationFrameworkHeader*)

    httpV2Client
      .post(url"$url")(hc)
      .withBody(data)
      .transform(_.withRequestTimeout(config.ifsTimeout))
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            debugLogs("submit event declaration report Event20A API 1829", url, hc.extraHeaders, data)
            response
          case _ =>
            handleErrorResponse("POST", url)(response)
        }
      }.andThen {
        postToAPIAuditService.sendSubmitEventDeclarationAuditEvent(pstr, data, reportVersion, Some(EventType.Event20A))
      }
  }

  def getVersions(pstr: String, reportType: String, startDate: String)
                 (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[JsArray] = {

    val url: String = config.versionUrl.format(pstr, reportType, startDate)

    implicit val hc: HeaderCarrier = headerCarrier.withExtraHeaders(integrationFrameworkHeader*)

    httpV2Client
      .get(url"$url")(hc)
      .transform(_.withRequestTimeout(config.ifsTimeout))
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            debugLogs("get versions", url, hc.extraHeaders, Json.obj())
            response.json.as[JsArray]
          case _ =>
            handleErrorResponse("GET", url)(response)
        }
    }
  }


  private def integrationFrameworkHeader: Seq[(String, String)] =
    Seq(
      "Environment"   -> config.integrationFrameworkEnvironment,
      "Authorization" -> config.integrationFrameworkAuthorization,
      "Content-Type"  -> "application/json",
      "CorrelationId" -> headerUtils.getCorrelationId
    )
}
