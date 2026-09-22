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
import models.admin.*
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
import uk.gov.hmrc.http.HttpVerbs.{GET, POST}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.mongoFeatureToggles.services.FeatureFlagService
import utils.HttpResponseHelper

import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZonedDateTime}
import java.util.{Base64, UUID}
import scala.concurrent.{ExecutionContext, Future}

class EventReportConnector @Inject()(
                                      config: AppConfig,
                                      httpV2Client: HttpClientV2,
                                      postToAPIAuditService: PostToAPIAuditService,
                                      featureFlagService: FeatureFlagService
                                    )
  extends HttpResponseHelper
    with Logging {

  private val token: String =
    Base64
      .getEncoder
      .encodeToString(s"${config.hipClientId}:${config.hipClientSecret}".getBytes(StandardCharsets.UTF_8))

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
                 (implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Seq[EROverview]] = {

    val url: String = config.overviewUrl.format(pstr, reportType, startDate, endDate)
    
    httpV2Client
      .get(url"$url")(hc.withExtraHeaders(connectorHeaders() *))
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
                Seq.empty[EROverview]
              case (_, Some(seqErr)) if seqErr.value.exists(jsValue => (jsValue \ "code").asOpt[String].contains("NO_REPORT_FOUND")) =>
                Seq.empty[EROverview]
              case _ =>
                println(s"\n\n\n\n${Json.parse(response.body)}\n\n\n\n\n")
                handleErrorResponse(GET, url)(response)
            }
          case _ =>
            handleErrorResponse(GET, url)(response)
        }
      }
  }

  private def getForApi(api: ApiType, eventType: Option[EventType], version: String, startDate: String, url: String, toggleEnabled: Boolean)
                       (implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[JsObject]] = {

    val logMessage =
      s"Get ${api.toString} called URL: $url. " +
        s"Event type: ${eventType.getOrElse(EventType.EventTypeNone)} " +
        s"reportStartDate: $startDate and reportVersionNumber: $version"

    httpV2Client
      .get(url"$url")(hc)
      .transform(_.withRequestTimeout(config.ifsTimeout))
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            debugLogs("get event API " + api.toString, url, hc.extraHeaders, response.json)
            Some(if (toggleEnabled) (response.json \ "success").as[JsObject] else response.json.as[JsObject])
          case NOT_FOUND | UNPROCESSABLE_ENTITY =>
            logger.warn(s"$logMessage and returned ${response.status} with message ${response.body}")
            None
          case _ =>
            handleErrorResponse(GET, url)(response)
        }
      }
  }

  def getEvent(pstr: String, startDate: String, version: String, eventType: Option[EventType])
              (implicit headerCarrier: HeaderCarrier, ec: ExecutionContext): Future[Option[JsObject]] = {
    val formattedVersion: String =
      s"00$version".takeRight(3)

    def hc(hipEnabled: Boolean, extraHeaders: Seq[(String, String)] = Seq.empty): HeaderCarrier =
      headerCarrier.withExtraHeaders(
        connectorHeaders(hipEnabled) ++
          Seq("reportStartDate" -> startDate, "reportVersionNumber" -> formattedVersion) ++
          extraHeaders *
      )

    eventType match {
      case Some(et) =>
        getApiTypeByEventType(et) match {
          case Some(api) =>
            api match {
              case Api1831 =>
                featureFlagService.get(Api1831HipMigrationToggle).flatMap { toggle =>
                  getForApi(api, eventType, formattedVersion, startDate, config.apiUrl(Api1831, toggle.isEnabled).format(pstr), toggle.isEnabled)(hc(toggle.isEnabled))
                }
              case Api1832 =>
                featureFlagService.get(Api1832HipMigrationToggle).flatMap { toggle =>
                  getForApi(api, eventType, formattedVersion, startDate, config.apiUrl(Api1832, toggle.isEnabled).format(pstr), toggle.isEnabled)(hc(toggle.isEnabled, Seq("eventType" -> s"Event${et.toString}")))
                }
              case Api1833 =>
                featureFlagService.get(Api1833HipMigrationToggle).flatMap { toggle =>
                  getForApi(api, eventType, formattedVersion, startDate, config.apiUrl(Api1833, toggle.isEnabled).format(pstr), toggle.isEnabled)(hc(toggle.isEnabled))
                }
              case Api1834 =>
                featureFlagService.get(Api1834HipMigrationToggle).flatMap { toggle =>
                  getForApi(api, eventType, formattedVersion, startDate, config.apiUrl(Api1834, toggle.isEnabled).format(pstr), toggle.isEnabled)(hc(toggle.isEnabled, Seq("eventType" -> s"Event${et.toString}")))
                }
              case _ =>
                Future.successful(None)
            }
          case None =>
            Future.successful(None)
        }
      case _ =>
        featureFlagService.get(Api1834HipMigrationToggle).flatMap { toggle =>
          getForApi(Api1834, eventType, formattedVersion, startDate, config.apiUrl(Api1834, toggle.isEnabled).format(pstr), toggle.isEnabled)(hc(toggle.isEnabled))
        }
    }
  }

  def compileEventReportSummary(psaPspId: String, pstr: String, data: JsValue, reportVersion: String)
                               (implicit hc: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] =
    featureFlagService.get(Api1826HipMigrationToggle).flatMap { toggle =>
      val url: String = config.apiUrl(Api1826, toggle.isEnabled).format(pstr)

      httpV2Client
        .post(url"$url")(hc.withExtraHeaders(connectorHeaders() *))
        .withBody(data)
        .transform(_.withRequestTimeout(config.ifsTimeout))
        .execute[HttpResponse]
        .map { response =>
          response.status match {
            case OK =>
              debugLogs("compile event report summary ", url, hc.extraHeaders, data)
              if (toggle.isEnabled) {
                (response.json \ "success").validate[JsObject] match {
                  case JsSuccess(value, path) =>
                    HttpResponse(status = OK, json = value, headers = response.headers)
                  case JsError(errors) =>
                    throw HttpException(errors.mkString("\n"), BAD_REQUEST)
                }
              } else {
                response
              }
            case _ =>
              handleErrorResponse(POST, url)(response)
          }
        }
    }
    .andThen {
      postToAPIAuditService.sendCompileEventDeclarationAuditEvent(psaPspId, pstr, data, reportVersion)
    }

  def compileEventOneReport(psaPspId: String, pstr: String, data: JsValue, reportVersion: String)
                           (implicit hc: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] =
    featureFlagService.get(Api1827HipMigrationToggle).flatMap { toggle =>
      val url: String = config.apiUrl(Api1827, toggle.isEnabled).format(pstr)

      httpV2Client
        .post(url"$url")(hc.withExtraHeaders(connectorHeaders(toggle.isEnabled) *))
        .withBody(data)
        .transform(_.withRequestTimeout(config.ifsTimeout))
        .execute[HttpResponse]
        .map { response =>
          response.status match {
            case OK =>
              debugLogs("compile event 1 API 1827", url, hc.extraHeaders, data)
              if (toggle.isEnabled) {
                (response.json \ "successes").validate[JsObject] match {
                  case JsSuccess(value, path) =>
                    HttpResponse(status = OK, json = value, headers = response.headers)
                  case JsError(errors) =>
                    throw HttpException(errors.mkString("\n"), BAD_REQUEST)
                }
              } else {
                response
              }
            case _ =>
              handleErrorResponse(POST, url)(response)
          }
        }
      }
      .andThen {
        postToAPIAuditService.sendCompileEventDeclarationAuditEvent(psaPspId, pstr, data, reportVersion)
      }

  def compileMemberEventReport(psaPspId: String, pstr: String, data: JsValue, reportVersion: String)
                              (implicit hc: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] =
    featureFlagService.get(Api1830HipMigrationToggle).flatMap { toggle =>
      val url: String = config.apiUrl(Api1830, toggle.isEnabled).format(pstr)

      httpV2Client
        .post(url"$url")(hc.withExtraHeaders(connectorHeaders(toggle.isEnabled) *))
        .withBody(data)
        .transform(_.withRequestTimeout(config.ifsTimeout))
        .execute[HttpResponse]
        .map { response =>
          response.status match {
            case OK =>
              debugLogs("compile Member Event API 1830", url, hc.extraHeaders, data)
              if (toggle.isEnabled) {
                (response.json \ "success").validate[JsObject] match {
                  case JsSuccess(value, path) =>
                    HttpResponse(status = OK, json = value, headers = response.headers)
                  case JsError(errors) =>
                    throw HttpException(errors.mkString("\n"), BAD_REQUEST)
                }
              } else {
                response
              }
            case _ =>
              handleErrorResponse(POST, url)(response)
          }
        }
      }
      .andThen {
        postToAPIAuditService.sendCompileEventDeclarationAuditEvent(psaPspId, pstr, data, reportVersion)
      }

  def submitEventDeclarationReport(pstr: String, data: JsValue, reportVersion: String)
                                  (implicit hc: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] =
    featureFlagService.get(Api1828HipMigrationToggle).flatMap { toggle =>
      val url: String = config.apiUrl(Api1828, toggle.isEnabled).format(pstr)

      httpV2Client
        .post(url"$url")(hc.withExtraHeaders(connectorHeaders(toggle.isEnabled) *))
        .withBody(data)
        .transform(_.withRequestTimeout(config.ifsTimeout))
        .execute[HttpResponse]
        .map { response =>
          response.status match {
            case OK =>
              debugLogs("submit event declaration report API 1828", url, hc.extraHeaders, data)
              if (toggle.isEnabled) {
                (response.json \ "success").validate[JsObject] match {
                  case JsSuccess(value, path) =>
                    HttpResponse(status = OK, json = value, headers = response.headers)
                  case JsError(errors) =>
                    throw HttpException(errors.mkString("\n"), BAD_REQUEST)
                }
              } else {
                response
              }
            case _ =>
              handleErrorResponse(POST, url)(response)
          }
        }
    }
    .andThen {
      postToAPIAuditService.sendSubmitEventDeclarationAuditEvent(pstr, data, reportVersion, None)
    }

  def submitEvent20ADeclarationReport(pstr: String, data: JsValue, reportVersion: String)
                                     (implicit hc: HeaderCarrier, ec: ExecutionContext, request: RequestHeader): Future[HttpResponse] =
    featureFlagService.get(Api1829HipMigrationToggle).flatMap { toggle =>
      val url: String = config.apiUrl(Api1829, toggle.isEnabled).format(pstr)
      
      httpV2Client
        .post(url"$url")(hc.withExtraHeaders(connectorHeaders(toggle.isEnabled) *))
        .withBody(data)
        .transform(_.withRequestTimeout(config.ifsTimeout))
        .execute[HttpResponse]
        .map { response =>
          response.status match {
            case OK =>
              debugLogs("submit event declaration report Event20A API 1829", url, hc.extraHeaders, data)
              if (toggle.isEnabled) {
                (response.json \ "success").validate[JsObject] match {
                  case JsSuccess(value, path) =>
                    HttpResponse(status = OK, json = value, headers = response.headers)
                  case JsError(errors) =>
                    throw HttpException(errors.mkString("\n"), BAD_REQUEST)
                }
              } else {
                response
              }
            case _ =>
              handleErrorResponse(POST, url)(response)
          }
        }
      }
      .andThen {
        postToAPIAuditService.sendSubmitEventDeclarationAuditEvent(pstr, data, reportVersion, Some(EventType.Event20A))
      }

  def getVersions(pstr: String, reportType: String, startDate: String)
                 (implicit hc: HeaderCarrier, ec: ExecutionContext): Future[JsArray] = {

    val url: String = config.versionUrl.format(pstr, reportType, startDate)
    
    httpV2Client
      .get(url"$url")(hc.withExtraHeaders(connectorHeaders() *))
      .transform(_.withRequestTimeout(config.ifsTimeout))
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            debugLogs("get versions", url, hc.extraHeaders, Json.obj())
            response.json.as[JsArray]
          case _ =>
            handleErrorResponse(GET, url)(response)
        }
    }
  }

  private val xReceiptDate: String =
    ZonedDateTime
      .ofInstant(Instant.now(), ZoneId.of("UTC"))
      .withNano(0)
      .format(DateTimeFormatter.ISO_INSTANT)

  private def connectorHeaders(hipEnabled: Boolean = false): Seq[(String, String)] =
    if (hipEnabled) {
      Seq(
        "X-Transmitting-System" -> "HIP",
        "X-Originating-System"  -> "MDTP",
        "X-Receipt-Date"        -> xReceiptDate,
        "correlationid"         -> UUID.randomUUID().toString,
        "Authorization"         -> s"Basic $token"
      )
    } else {
      Seq(
        "Environment"   -> config.integrationFrameworkEnvironment,
        "Authorization" -> config.integrationFrameworkAuthorization,
        "Content-Type"  -> "application/json",
        "CorrelationId" -> UUID.randomUUID().toString
      )
    }
}
