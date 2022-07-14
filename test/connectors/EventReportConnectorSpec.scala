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
import models.enumeration.EventType.Event3
import models.{EROverview, EROverviewVersion, ERVersion}
import org.mockito.MockitoSugar
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import play.api.http.Status._
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.{JsArray, JsString, Json}
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.test.HttpClientSupport
import utils.{JsonFileReader, UnrecognisedHttpResponseException, WireMockHelper}

import java.time.LocalDate


class EventReportConnectorSpec extends AsyncWordSpec with Matchers with WireMockHelper with HttpClientSupport with JsonFileReader with MockitoSugar {

  import EventReportConnectorSpec._

  private implicit lazy val hc: HeaderCarrier = HeaderCarrier()

  override protected def portConfigKeys: String = "microservice.services.if-hod.port,microservice.services.des-hod.port"

  private val mockHeaderUtils = mock[HeaderUtils]
  private lazy val connector: EventReportConnector = injector.instanceOf[EventReportConnector]

  override protected def bindings: Seq[GuiceableModule] =
    Seq(
      bind[HttpClient].toInstance(httpClient),
      bind[HeaderUtils].toInstance(mockHeaderUtils)
    )

  override def beforeEach(): Unit = {
    when(mockHeaderUtils.getCorrelationId).thenReturn(testCorrelationId)
    super.beforeEach()
  }

  private def errorResponse(code: String): String = {
    Json.stringify(
      Json.obj(
        "code" -> code,
        "reason" -> s"Reason for $code"
      )
    )
  }

  private def seqErrorResponse(code: String): String = {
    Json.stringify(
      Json.obj(
        "failures" ->
          Json.arr(
            Json.obj(
              "code" -> code,
              "reason" -> s"Reason for $code"
            )
          )
      )
    )
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

  "compileEventOneReport" must {

    "return successfully when ETMP has returned OK" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileEventOneReportUrl))
          .withHeader("Content-Type", equalTo("application/json"))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            ok
          )
      )
      connector.compileEventOneReport(pstr, data) map {
        _.status mustBe OK
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileEventOneReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            badRequest().withBody("INVALID_PAYLOAD")
          )
      )
      recoverToExceptionIf[BadRequestException] {
        connector.compileEventOneReport(pstr, data)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException without Invalid " in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileEventOneReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            badRequest()
          )
      )
      recoverToExceptionIf[BadRequestException] {
        connector.compileEventOneReport(pstr, data)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return NOT FOUND when ETMP has returned NotFoundException" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileEventOneReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            notFound()
          )
      )

      recoverToExceptionIf[NotFoundException] {
        connector.compileEventOneReport(pstr, data)
      } map {
        _.responseCode mustEqual NOT_FOUND
      }
    }

    "return Upstream5xxResponse when ETMP has returned Internal Server Error" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileEventOneReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            serverError()
          )
      )
      recoverToExceptionIf[UpstreamErrorResponse](connector.compileEventOneReport(pstr, data)) map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "return 4xx when ETMP has returned Upstream error response" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileEventOneReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            forbidden()
          )
      )
      recoverToExceptionIf[UpstreamErrorResponse](connector.compileEventOneReport(pstr, data)) map {
        _.statusCode mustBe FORBIDDEN
      }
    }

    "return 204 when ETMP has returned Unrecognized http response" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileEventOneReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            noContent()
          )
      )
      recoverToExceptionIf[UnrecognisedHttpResponseException](connector.compileEventOneReport(pstr, data)) map { response =>
        response.getMessage must include("204")
      }
    }
  }

  "getOverview" must {
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
      connector.getOverview(pstr, reportTypeER, fromDt, toDt).map { response =>
        response mustBe erOverview
      }
    }

    "return a Seq.empty for NOT FOUND - 404 with response code NO_REPORT_FOUND" in {
      server.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(
            notFound
              .withBody(errorResponse("NO_REPORT_FOUND"))
          )
      )

      connector.getOverview(pstr, reportTypeER, fromDt, toDt).map { response =>
        response mustEqual Seq.empty
      }
    }

    "return a Seq.empty for 404 with response code NO_REPORT_FOUND in a sequence of errors" in {
      server.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(
            notFound
              .withBody(seqErrorResponse("NO_REPORT_FOUND"))
          )
      )

      connector.getOverview(pstr, reportTypeER, fromDt, toDt).map { response =>
        response mustEqual Seq.empty
      }
    }

    "return a NotFoundException for a 404 response code without NO_REPORT_FOUND in a sequence of errors" in {
      server.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(
            notFound
              .withBody(seqErrorResponse("SOME_OTHER_ERROR"))
          )
      )

      recoverToExceptionIf[NotFoundException] {
        connector.getOverview(pstr, reportTypeER, fromDt, toDt)
      } map { response =>
        response.responseCode mustEqual NOT_FOUND
        response.message must include("SOME_OTHER_ERROR")
      }
    }

    "return a NotFoundException for NOT FOUND - 404" in {
      server.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(
            notFound
              .withBody(errorResponse("NOT_FOUND"))
          )
      )

      recoverToExceptionIf[NotFoundException] {
        connector.getOverview(pstr, reportTypeER, fromDt, toDt)
      } map { response =>
        response.responseCode mustEqual NOT_FOUND
        response.message must include("NOT_FOUND")
      }
    }

    "throw Upstream4XX for FORBIDDEN - 403" in {

      server.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(
            forbidden
              .withBody(errorResponse("FORBIDDEN"))
          )
      )
      recoverToExceptionIf[UpstreamErrorResponse](connector.getOverview(pstr, reportTypeER, fromDt, toDt)) map {
        ex =>
          ex.statusCode mustBe FORBIDDEN
          ex.message must include("FORBIDDEN")
      }
    }

    "throw Upstream5XX for INTERNAL SERVER ERROR - 500" in {

      server.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(
            serverError
              .withBody(errorResponse("SERVER_ERROR"))
          )
      )

      recoverToExceptionIf[UpstreamErrorResponse](connector.getOverview(pstr, reportTypeER, fromDt, toDt)) map {
        ex =>
          ex.statusCode mustBe INTERNAL_SERVER_ERROR
          ex.message must include("SERVER_ERROR")
      }
    }
  }




  "getEvent" must {
    "return the json returned from ETMP for valid event in API 1832" in {
      val response: JsString = JsString("test")

      server.stubFor(
        get(urlEqualTo(getEventUrl))
          .willReturn(
            ok
              .withHeader("Content-Type", "application/json")
              .withBody(response.toString())
          )
      )

      connector.getEvent(pstr, fromDt, "001", Event3).map { response =>
        response mustBe response
      }
    }

    "return a NotFoundException for NOT FOUND - 404" in {
      server.stubFor(
        get(urlEqualTo(getEventUrl))
          .willReturn(
            notFound
              .withBody(errorResponse("NOT_FOUND"))
          )
      )

      recoverToExceptionIf[NotFoundException] {
        connector.getEvent(pstr, fromDt, "001", Event3)
      } map { response =>
        response.responseCode mustEqual NOT_FOUND
        response.message must include("NOT_FOUND")
      }
    }



    "throw Upstream5XX for INTERNAL SERVER ERROR - 500" in {

      server.stubFor(
        get(urlEqualTo(getEventUrl))
          .willReturn(
            serverError
              .withBody(errorResponse("SERVER_ERROR"))
          )
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.getEvent(pstr, fromDt, "001", Event3)
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }
  }


  "submitEventDeclarationReport" must {
    "return 200 when ETMP has returned OK" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(submitEventDeclarationReportUrl))
          .withHeader("Content-Type", equalTo("application/json"))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            ok
          )
      )
      connector.submitEventDeclarationReport(pstr, data) map {
        _.status mustBe OK
      }
    }

    "return Upstream5xxResponse when ETMP has returned Internal Server Error" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(submitEventDeclarationReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            serverError()
          )
      )
      recoverToExceptionIf[UpstreamErrorResponse](connector.submitEventDeclarationReport(pstr, data)) map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "return 400 when ETMP has returned BadRequestException" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(submitEventDeclarationReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            badRequest().withBody("INVALID_PAYLOAD")
          )
      )
      recoverToExceptionIf[BadRequestException] {
        connector.submitEventDeclarationReport(pstr, data)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }
  }

  "return BAD REQUEST when ETMP has returned BadRequestException without Invalid " in {
    val data = Json.obj(fields = "Id" -> "value")
    server.stubFor(
      post(urlEqualTo(submitEventDeclarationReportUrl))
        .withRequestBody(equalTo(Json.stringify(data)))
        .willReturn(
          badRequest()
        )
    )
    recoverToExceptionIf[BadRequestException] {
      connector.submitEventDeclarationReport(pstr, data)
    } map {
      _.responseCode mustEqual BAD_REQUEST
    }
  }

  "return NOT FOUND when ETMP has returned NotFoundException" in {
    val data = Json.obj(fields = "Id" -> "value")
    server.stubFor(
      post(urlEqualTo(submitEventDeclarationReportUrl))
        .withRequestBody(equalTo(Json.stringify(data)))
        .willReturn(
          notFound()
        )
    )

    recoverToExceptionIf[NotFoundException] {
      connector.submitEventDeclarationReport(pstr, data)
    } map {
      _.responseCode mustEqual NOT_FOUND
    }
  }

  "return 4xx when ETMP has returned Upstream error response" in {
    val data = Json.obj(fields = "Id" -> "value")
    server.stubFor(
      post(urlEqualTo(submitEventDeclarationReportUrl))
        .withRequestBody(equalTo(Json.stringify(data)))
        .willReturn(
          forbidden()
        )
    )
    recoverToExceptionIf[UpstreamErrorResponse](connector.submitEventDeclarationReport(pstr, data)) map {
      _.statusCode mustBe FORBIDDEN
    }
  }

  "return 204 when ETMP has returned Unrecognized http response" in {
    val data = Json.obj(fields = "Id" -> "value")
    server.stubFor(
      post(urlEqualTo(submitEventDeclarationReportUrl))
        .withRequestBody(equalTo(Json.stringify(data)))
        .willReturn(
          noContent()
        )
    )
    recoverToExceptionIf[UnrecognisedHttpResponseException](connector.submitEventDeclarationReport(pstr, data)) map { response =>
      response.getMessage must include("204")
    }
  }

  "getVersions" must {
    "return successfully when DES has returned OK" in {

      server.stubFor(
        get(urlEqualTo(getErVersionUrl(reportTypeER)))
          .willReturn(
            ok
              .withHeader("Content-Type", "application/json")
              .withBody(erVersionResponseJson.toString())
          )
      )
      connector.getVersions(pstr, reportTypeER, startDate = startDt).map { response =>
        response mustBe erVersions
      }
    }

    "throw NotFoundException" in {

      server.stubFor(
        get(urlEqualTo(getErVersionUrl(reportTypeER)))
          .willReturn(
            badRequest()
          )
      )
      recoverToExceptionIf[NotFoundException](connector.getVersions(pstr, reportTypeER, "")) map {
        ex =>
          ex.responseCode mustBe NOT_FOUND
      }
    }

  }

  "compileMemberEventReport" must {

    "return successfully when ETMP has returned OK" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileMemberEventReportUrl))
          .withHeader("Content-Type", equalTo("application/json"))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            ok
          )
      )
      connector.compileMemberEventReport(pstr, data) map {
        _.status mustBe OK
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileMemberEventReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            badRequest().withBody("INVALID_PAYLOAD")
          )
      )
      recoverToExceptionIf[BadRequestException] {
        connector.compileMemberEventReport(pstr, data)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException without Invalid " in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileMemberEventReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            badRequest()
          )
      )
      recoverToExceptionIf[BadRequestException] {
        connector.compileMemberEventReport(pstr, data)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return NOT FOUND when ETMP has returned NotFoundException" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileMemberEventReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            notFound()
          )
      )

      recoverToExceptionIf[NotFoundException] {
        connector.compileMemberEventReport(pstr, data)
      } map {
        _.responseCode mustEqual NOT_FOUND
      }
    }

    "return Upstream5xxResponse when ETMP has returned Internal Server Error" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileMemberEventReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            serverError()
          )
      )
      recoverToExceptionIf[UpstreamErrorResponse](connector.compileMemberEventReport(pstr, data)) map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "return 4xx when ETMP has returned Upstream error response" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileMemberEventReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            forbidden()
          )
      )
      recoverToExceptionIf[UpstreamErrorResponse](connector.compileMemberEventReport(pstr, data)) map {
        _.statusCode mustBe FORBIDDEN
      }
    }

    "return 204 when ETMP has returned Unrecognized http response" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(compileMemberEventReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            noContent()
          )
      )
      recoverToExceptionIf[UnrecognisedHttpResponseException](connector.compileMemberEventReport(pstr, data)) map { response =>
        response.getMessage must include("204")
      }
    }
  }

  "submitEvent20ADeclarationReport" must {

    "return successfully when ETMP has returned OK" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(submitEvent20ADeclarationReportUrl))
          .withHeader("Content-Type", equalTo("application/json"))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            ok
          )
      )
      connector.submitEvent20ADeclarationReport(pstr, data) map {
        _.status mustBe OK
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(submitEvent20ADeclarationReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            badRequest().withBody("INVALID_PAYLOAD")
          )
      )
      recoverToExceptionIf[BadRequestException] {
        connector.submitEvent20ADeclarationReport(pstr, data)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException without Invalid " in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(submitEvent20ADeclarationReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            badRequest()
          )
      )
      recoverToExceptionIf[BadRequestException] {
        connector.submitEvent20ADeclarationReport(pstr, data)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return NOT FOUND when ETMP has returned NotFoundException" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(submitEvent20ADeclarationReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            notFound()
          )
      )

      recoverToExceptionIf[NotFoundException] {
        connector.submitEvent20ADeclarationReport(pstr, data)
      } map {
        _.responseCode mustEqual NOT_FOUND
      }
    }

    "return Upstream5xxResponse when ETMP has returned Internal Server Error" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(submitEvent20ADeclarationReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            serverError()
          )
      )
      recoverToExceptionIf[UpstreamErrorResponse](connector.submitEvent20ADeclarationReport(pstr, data)) map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "return 4xx when ETMP has returned Upstream error response" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(submitEvent20ADeclarationReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            forbidden()
          )
      )
      recoverToExceptionIf[UpstreamErrorResponse](connector.submitEvent20ADeclarationReport(pstr, data)) map {
        _.statusCode mustBe FORBIDDEN
      }
    }

    "return 204 when ETMP has returned Unrecognized http response" in {
      val data = Json.obj(fields = "Id" -> "value")
      server.stubFor(
        post(urlEqualTo(submitEvent20ADeclarationReportUrl))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(
            noContent()
          )
      )
      recoverToExceptionIf[UnrecognisedHttpResponseException](connector.submitEvent20ADeclarationReport(pstr, data)) map { response =>
        response.getMessage must include("204")
      }
    }
  }

}

object EventReportConnectorSpec {
  private val pstr = "test-pstr"
  private val reportTypeER = "ER"
  private val fromDt = "2022-04-06"
  private val toDt = "2022-04-05"
  private val testCorrelationId = "testCorrelationId"
  private val eventReportSummaryUrl = s"/pension-online/event-reports/pods/$pstr"
  private val compileEventOneReportUrl = s"/pension-online/event1-report/pods/$pstr"
  private val compileMemberEventReportUrl = s"/pension-online/member-event-reports/$pstr"
  private val submitEvent20ADeclarationReportUrl = s"/pension-online/event-20a-declaration-report/$pstr"

  private val getErOverviewUrl = s"/pension-online/reports/overview/pods/$pstr/ER?fromDate=$fromDt&toDate=$toDt"
  private val getEventUrl = s"/pension-online/member-event-status-reports/$pstr"

  private val submitEventDeclarationReportUrl = s"/pension-online/event-declaration-reports/$pstr"


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

  private val startDt = "2022-04-01"
  private def getErVersionUrl(reportType: String) = s"/pension-online/reports/$pstr/$reportType/versions?startDate=$startDt"
  private val erVersionResponseJson: JsArray = Json.arr(
    Json.obj(
      "reportFormBundleNumber" -> "123456789012",
      "reportVersion" -> 1,
      "reportStatus" -> "Compiled",
      "compilationOrSubmissionDate" -> s"${startDt}T09:30:47Z",
      "reportSubmitterDetails" -> Json.obj(
        "reportSubmittedBy" -> "PSP",
        "orgOrPartnershipDetails" -> Json.obj(
          "orgOrPartnershipName" -> "ABC Limited"
        )
      ),
      "psaDetails" -> Json.obj(
        "psaOrgOrPartnershipDetails" -> Json.obj(
          "orgOrPartnershipName" -> "XYZ Limited"
        )
      )
    )
  )

  private val version = ERVersion( 1,
    LocalDate.of(2022, 4, 1),
    "Compiled")
  private val erVersions = Seq(version)
}

