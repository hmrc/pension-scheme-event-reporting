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

import com.github.tomakehurst.wiremock.client.WireMock._
import org.mockito.MockitoSugar
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import play.api.http.Status._
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.Json
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.{JsonFileReader, UnrecognisedHttpResponseException, WireMockHelper}

class EventReportConnectorSpec extends AsyncWordSpec with Matchers with WireMockHelper with HttpClientSupport  with JsonFileReader with MockitoSugar {

  private implicit lazy val hc: HeaderCarrier = HeaderCarrier()

  override protected def portConfigKeys: String = "microservice.services.if-hod.port"

  private val mockHeaderUtils = mock[HeaderUtils]
  private lazy val connector: EventReportConnector = injector.instanceOf[EventReportConnector]

  override protected def bindings: Seq[GuiceableModule] =
    Seq(
      bind[HttpClient].toInstance(httpClient),
      bind[HeaderUtils].toInstance(mockHeaderUtils)
    )

  private val pstr = "test-pstr"
  private val eventReportSummaryUrl = s"/pension-online/event-reports/pods/$pstr"
  private val testCorrelationId = "testCorrelationId"

  override def beforeEach(): Unit = {
    when(mockHeaderUtils.getCorrelationId).thenReturn(testCorrelationId)
    super.beforeEach()
  }

  "compileEventReportSummary" must {

    "return successfully when ETMP has returned OK" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(eventReportSummaryUrl))
          .withHeader("Content-Type", equalTo("application/json"))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            ok
          )
      )
      connector.compileEventReportSummary(pstr, data) map {
        _.status mustBe OK
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(eventReportSummaryUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            badRequest().withBody("INVALID_PAYLOAD")
          )
      )
      recoverToExceptionIf[BadRequestException] {
        connector.compileEventReportSummary(pstr, data)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException without Invalid " in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(eventReportSummaryUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            badRequest()
          )
      )
      recoverToExceptionIf[BadRequestException] {
        connector.compileEventReportSummary(pstr, data)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return NOT FOUND when ETMP has returned NotFoundException" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(eventReportSummaryUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            notFound()
          )
      )

      recoverToExceptionIf[NotFoundException] {
        connector.compileEventReportSummary(pstr, data)
      } map {
        _.responseCode mustEqual NOT_FOUND
      }
    }

    "return Upstream5xxResponse when ETMP has returned Internal Server Error" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(eventReportSummaryUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            serverError()
          )
      )
      recoverToExceptionIf[UpstreamErrorResponse](connector.compileEventReportSummary(pstr, data)) map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "return 4xx when ETMP has returned Upstream error response" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(eventReportSummaryUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            forbidden()
          )
      )
      recoverToExceptionIf[UpstreamErrorResponse](connector.compileEventReportSummary(pstr, data)) map {
        _.statusCode mustBe FORBIDDEN
      }
    }

    "return 204 when ETMP has returned Unrecognized http response" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(eventReportSummaryUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            noContent()
          )
      )
      recoverToExceptionIf[UnrecognisedHttpResponseException](connector.compileEventReportSummary(pstr, data)) map { response =>
        response.getMessage must include("204")
      }
    }

  }
}

