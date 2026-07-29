/*
 * Copyright 2026 HM Revenue & Customs
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

import actions.AuthRequest
import config.AppConfig
import models.MinimalDetails
import play.api.http.Status.{FORBIDDEN, OK}
import play.api.libs.json.Json
import uk.gov.hmrc.http.HttpVerbs.GET
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import utils.HttpResponseHelper

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class MinimalDetailsConnector @Inject() (
                                          httpClient: HttpClientV2,
                                          config: AppConfig
                                        )(implicit ec: ExecutionContext)
  extends HttpResponseHelper {

  val url = url"${config.minimalPsaDetailsUrl}"

  def getMinimalDetails[A](implicit hc: HeaderCarrier, request: AuthRequest[A]): Future[MinimalDetails] = {

    val hcWithId: HeaderCarrier =
      request.psaOrPspId match {
        case Left(_) =>
          hc.withExtraHeaders("loggedInAsPsa" -> "true")
        case Right(_) =>
          hc.withExtraHeaders("loggedInAsPsa" -> "false")
        case _ =>
          throw new Exception("Could not retrieve ID from request")
      }

    httpClient
      .get(url)(hcWithId)
      .setHeader(hcWithId.extraHeaders*)
      .transform(_.withRequestTimeout(config.ifsTimeout))
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            val minimalDetails =
              Json.parse(response.body).as[MinimalDetails]

            (minimalDetails.individualDetails, minimalDetails.organisationName) match {
              case (None, None) =>
                throw new Exception("Neither individualDetails or organisationName returned from min details")
              case _            =>
                minimalDetails
            }
          case FORBIDDEN if response.body.contains("DELIMITED_PSAID") =>
            throw new DelimitedAdminException
          case _ =>
            handleErrorResponse(GET, url.toString)(response)
        }
      }
  }
}

class DelimitedAdminException extends
  Exception("The administrator has already de-registered. The minimal details API has returned a DELIMITED PSA response")
