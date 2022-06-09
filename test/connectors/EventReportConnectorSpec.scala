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
import models.{EROverview, EROverviewVersion}
import org.mockito.MockitoSugar
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import play.api.http.Status._
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.{JsArray, Json}
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.{JsonFileReader, UnrecognisedHttpResponseException, WireMockHelper}

import java.time.LocalDate

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
  private val fromDt = "2022-04-06"
  private val toDt = "2022-04-05"
  private val eventReportSummaryUrl = s"/pension-online/event-reports/pods/$pstr"
  private val testCorrelationId = "testCorrelationId"
  private val getErOverviewUrl = s"/pension-online/reports/overview/pods/$pstr/ER?fromDate=$fromDt&toDate=$toDt"
  private val getErOverview20aUrl = s"/pension-online/reports/overview/pods/$pstr/ER20A?fromDate=$fromDt&toDate=$toDt"

  private val overview1 = EROverview(
    LocalDate.of(2022, 4, 6),
    LocalDate.of(2023, 4, 5),
    tpssReportPresent = false,
    Some(EROverviewVersion(
      3,
      submittedVersionAvailable = false,
      compiledVersionAvailable = true)))

  private val overview2 = EROverview(
    LocalDate.of(2022, 4, 6),
    LocalDate.of(2023, 4, 5),
    tpssReportPresent = false,
    Some(EROverviewVersion(
      2,
      submittedVersionAvailable = true,
      compiledVersionAvailable = true)))

  private val erOverview = Seq(overview1, overview2)

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

  "getErOverview" must {
    "return the seq of overviewDetails returned from the ETMP" in {
      val erOverviewResponseJson: JsArray = Json.arr(
        Json.obj(
          "periodStartDate" -> "2022-04-06",
          "periodEndDate" -> "2023-04-05",
            "numberOfVersions" -> 3,
            "submittedVersionAvailable" -> "No",
            "compiledVersionAvailable" -> "Yes"
          ),
        Json.obj(
          "periodStartDate" -> "2022-04-06",
          "periodEndDate" -> "2023-04-05",
            "numberOfVersions" -> 2,
            "submittedVersionAvailable" -> "Yes",
            "compiledVersionAvailable" -> "Yes"
          )
      )

      server.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(
            ok
              .withHeader("Content-Type", "application/json")
              .withBody(erOverviewResponseJson.toString())
          )
      )
      connector.getErOverview(pstr, fromDt, toDt).map { response =>
        response mustBe erOverview
      }
    }
  }
  "getErOverview20A" must {
    "return the seq of overviewDetails returned from the ETMP" in {
      val er20aOverviewResponseJson: JsArray = Json.arr(
        Json.obj(
          "periodStartDate" -> "2022-04-06",
          "periodEndDate" -> "2023-04-05",
            "numberOfVersions" -> 3,
            "submittedVersionAvailable" -> "No",
            "compiledVersionAvailable" -> "Yes"
          ),
        Json.obj(
          "periodStartDate" -> "2022-04-06",
          "periodEndDate" -> "2023-04-05",
            "numberOfVersions" -> 2,
            "submittedVersionAvailable" -> "Yes",
            "compiledVersionAvailable" -> "Yes"
          )
      )

      server.stubFor(
        get(urlEqualTo(getErOverview20aUrl))
          .willReturn(
            ok
              .withHeader("Content-Type", "application/json")
              .withBody(er20aOverviewResponseJson.toString())
          )
      )
      connector.getEr20AOverview(pstr, fromDt, toDt).map { response =>
        response mustBe erOverview
      }
    }
  }
}

