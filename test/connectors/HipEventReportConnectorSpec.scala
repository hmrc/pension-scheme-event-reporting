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

import com.github.tomakehurst.wiremock.client.WireMock.*
import models.admin.*
import models.enumeration.EventType.*
import models.{EROverview, EROverviewVersion}
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.{reset, times, verify, when}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.Application
import play.api.http.Status.*
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import play.api.mvc.RequestHeader
import play.api.test.FakeRequest
import repositories.EventReportCacheRepository
import services.PostToAPIAuditService
import uk.gov.hmrc.http.*
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.test.{HttpClientV2Support, WireMockSupport}
import uk.gov.hmrc.mongoFeatureToggles.model.FeatureFlag
import uk.gov.hmrc.mongoFeatureToggles.services.FeatureFlagService
import utils.UnrecognisedHttpResponseException

import java.time.LocalDate
import scala.concurrent.Future
import scala.util.Try


class HipEventReportConnectorSpec
  extends AsyncWordSpec
    with Matchers
    with WireMockSupport
    with HttpClientV2Support
    with MockitoSugar {

  import HipEventReportConnectorSpec.*

  private implicit lazy val hc: HeaderCarrier = HeaderCarrier()
  private implicit lazy val rh: RequestHeader = FakeRequest("", "")

  private val mockEventReportCacheRepository: EventReportCacheRepository =
    mock[EventReportCacheRepository]
  private val mockPostToAPIAuditService: PostToAPIAuditService =
    mock[PostToAPIAuditService]
  private val mockFeatureFlagService: FeatureFlagService =
    mock[FeatureFlagService]

  lazy val app: Application =
    GuiceApplicationBuilder()
      .configure(
        "microservice.services.hip-hod.port" -> wireMockPort,
        "microservice.services.if-hod.port" -> wireMockPort
      )
      .overrides(
        bind[HttpClientV2].toInstance(httpClientV2),
        bind[EventReportCacheRepository].toInstance(mockEventReportCacheRepository),
        bind[PostToAPIAuditService].toInstance(mockPostToAPIAuditService),
        bind[FeatureFlagService].toInstance(mockFeatureFlagService)
      )
      .build()

  private lazy val connector: EventReportConnector =
    app.injector.instanceOf[EventReportConnector]

  private val pfSuccess: PartialFunction[Try[HttpResponse], Unit] =
    new PartialFunction[Try[HttpResponse], Unit] {
      override def isDefinedAt(x: Try[HttpResponse]): Boolean = true

      override def apply(v1: Try[HttpResponse]): Unit = ()
    }


  override def beforeEach(): Unit = {
    reset(
      mockPostToAPIAuditService,
      mockFeatureFlagService
    )

    Seq(
      Api1826HipMigrationToggle,
      Api1827HipMigrationToggle,
      Api1828HipMigrationToggle,
      Api1829HipMigrationToggle,
      Api1830HipMigrationToggle,
      Api1831HipMigrationToggle,
      Api1832HipMigrationToggle,
      Api1833HipMigrationToggle,
      Api1834HipMigrationToggle
    ).foreach { toggle =>
      when(mockFeatureFlagService.get(toggle))
        .thenReturn(Future.successful(FeatureFlag(toggle, isEnabled = true)))
    }

    when(mockPostToAPIAuditService.sendSubmitEventDeclarationAuditEvent(any(), any(), any(), any())(any(), any()))
      .thenReturn(pfSuccess)
  }

  private def errorResponse(code: String): String =
    Json.stringify(
      Json.obj(
        "code" -> code,
        "reason" -> s"Reason for $code"
      )
    )

  private def seqErrorResponse(code: String): String =
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

  "compileEventReportSummary" must {
    val data = Json.obj("Id" -> "value")

    "return successfully when ETMP has returned OK" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1826Url))
          .withHeader("Content-Type", equalTo("application/json"))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(ok.withBody(Json.obj("success" -> Json.obj("key" -> "value")).toString))
      )

      connector.compileEventReportSummary(psaId, pstr, data, reportVersion).map {
        verify(mockPostToAPIAuditService, times(1))
          .sendCompileEventDeclarationAuditEvent(any(), eqTo(pstr), eqTo(data), eqTo(reportVersion))(any(), any())
        _.status mustBe OK
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1826Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(badRequest().withBody("INVALID_PAYLOAD"))
      )

      recoverToExceptionIf[BadRequestException] {
        connector.compileEventReportSummary(psaId, pstr, data, reportVersion)
      }.map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException without Invalid " in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1826Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(badRequest())
      )

      recoverToExceptionIf[BadRequestException] {
        connector.compileEventReportSummary(psaId, pstr, data, reportVersion)
      }.map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return NOT FOUND when ETMP has returned NotFoundException" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1826Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(notFound())
      )

      recoverToExceptionIf[NotFoundException] {
        connector.compileEventReportSummary(psaId, pstr, data, reportVersion)
      }.map {
        _.responseCode mustEqual NOT_FOUND
      }
    }

    "return Upstream5xxResponse when ETMP has returned Internal Server Error" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1826Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(serverError())
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.compileEventReportSummary(psaId, pstr, data, reportVersion)
      }.map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "return 4xx when ETMP has returned Upstream error response" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1826Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(forbidden())
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.compileEventReportSummary(psaId, pstr, data, reportVersion)
      }.map {
        _.statusCode mustBe FORBIDDEN
      }
    }

    "return 204 when ETMP has returned Unrecognized http response" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1826Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(noContent())
      )

      recoverToExceptionIf[UnrecognisedHttpResponseException] {
        connector.compileEventReportSummary(psaId, pstr, data, reportVersion)
      }.map {
        _.getMessage must include("204")
      }
    }
  }

  "compileEventOneReport" must {
    val data = Json.obj("Id" -> "value")
    "return successfully when ETMP has returned OK" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1827Url))
          .withHeader("Content-Type", equalTo("application/json"))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(ok.withBody(Json.obj("successes" -> Json.obj("key" -> "value")).toString))
      )
      connector.compileEventOneReport(psaId, pstr, data, reportVersion).map {
        verify(mockPostToAPIAuditService, times(1))
          .sendCompileEventDeclarationAuditEvent(any(), eqTo(pstr), eqTo(data), eqTo(reportVersion))(any(), any())
        _.status mustBe OK
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1827Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(badRequest().withBody("INVALID_PAYLOAD"))
      )

      recoverToExceptionIf[BadRequestException] {
        connector.compileEventOneReport(psaId, pstr, data, reportVersion)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException without Invalid " in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1827Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(badRequest())
      )

      recoverToExceptionIf[BadRequestException] {
        connector.compileEventOneReport(psaId, pstr, data, reportVersion)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return NOT FOUND when ETMP has returned NotFoundException" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1827Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(notFound())
      )

      recoverToExceptionIf[NotFoundException] {
        connector.compileEventOneReport(psaId, pstr, data, reportVersion)
      } map {
        _.responseCode mustEqual NOT_FOUND
      }
    }

    "return Upstream5xxResponse when ETMP has returned Internal Server Error" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1827Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(serverError())
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.compileEventOneReport(psaId, pstr, data, reportVersion)
      }.map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "return 4xx when ETMP has returned Upstream error response" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1827Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(forbidden())
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.compileEventOneReport(psaId, pstr, data, reportVersion)
      }.map {
        _.statusCode mustBe FORBIDDEN
      }
    }

    "return 204 when ETMP has returned Unrecognized http response" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1827Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(noContent())
      )

      recoverToExceptionIf[UnrecognisedHttpResponseException] {
        connector.compileEventOneReport(psaId, pstr, data, reportVersion)
      }.map {
        _.getMessage must include("204")
      }
    }
  }

  "compileMemberEventReport" must {
    val data = Json.obj("Id" -> "value")

    "return successfully when ETMP has returned OK" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1830Url))
          .withHeader("Content-Type", equalTo("application/json"))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(ok.withBody(Json.obj("success" -> Json.obj("key" -> "value")).toString))
      )
      connector.compileMemberEventReport(psaId, pstr, data, reportVersion).map {
        verify(mockPostToAPIAuditService, times(1))
          .sendCompileEventDeclarationAuditEvent(any(), eqTo(pstr), eqTo(data), eqTo(reportVersion))(any(), any())
        _.status mustBe OK
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1830Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(badRequest().withBody("INVALID_PAYLOAD"))
      )

      recoverToExceptionIf[BadRequestException] {
        connector.compileMemberEventReport(psaId, pstr, data, reportVersion)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException without Invalid " in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1830Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(badRequest())
      )

      recoverToExceptionIf[BadRequestException] {
        connector.compileMemberEventReport(psaId, pstr, data, reportVersion)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return NOT FOUND when ETMP has returned NotFoundException" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1830Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(notFound())
      )

      recoverToExceptionIf[NotFoundException] {
        connector.compileMemberEventReport(psaId, pstr, data, reportVersion)
      } map {
        _.responseCode mustEqual NOT_FOUND
      }
    }

    "return Upstream5xxResponse when ETMP has returned Internal Server Error" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1830Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(serverError())
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.compileMemberEventReport(psaId, pstr, data, reportVersion)
      }.map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "return 4xx when ETMP has returned Upstream error response" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1830Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(forbidden())
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.compileMemberEventReport(psaId, pstr, data, reportVersion)
      }.map {
        _.statusCode mustBe FORBIDDEN
      }
    }

    "return 204 when ETMP has returned Unrecognized http response" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1830Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(noContent())
      )

      recoverToExceptionIf[UnrecognisedHttpResponseException] {
        connector.compileMemberEventReport(psaId, pstr, data, reportVersion)
      }.map {
        _.getMessage must include("204")
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

      wireMockServer.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(
            ok
              .withHeader("Content-Type", "application/json")
              .withBody(erOverviewResponseJson.toString())
          )
      )

      connector.getOverview(pstr, reportTypeER, fromDt, toDt).map { response =>
        response mustBe Seq(overview1, overview2)
      }
    }

    "return a Seq.empty for NOT FOUND - 404 with response code NO_REPORT_FOUND" in {
      wireMockServer.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(notFound.withBody(errorResponse("NO_REPORT_FOUND")))
      )

      connector.getOverview(pstr, reportTypeER, fromDt, toDt).map { response =>
        response mustEqual Seq.empty
      }
    }

    "return a Seq.empty for 404 with response code NO_REPORT_FOUND in a sequence of errors" in {
      wireMockServer.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(notFound.withBody(seqErrorResponse("NO_REPORT_FOUND")))
      )

      connector.getOverview(pstr, reportTypeER, fromDt, toDt).map { response =>
        response mustEqual Seq.empty
      }
    }

    "return a NotFoundException for a 404 response code without NO_REPORT_FOUND in a sequence of errors" in {
      wireMockServer.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(notFound.withBody(seqErrorResponse("SOME_OTHER_ERROR")))
      )

      recoverToExceptionIf[NotFoundException] {
        connector.getOverview(pstr, reportTypeER, fromDt, toDt)
      } map { response =>
        response.responseCode mustEqual NOT_FOUND
        response.message must include("SOME_OTHER_ERROR")
      }
    }

    "return a NotFoundException for NOT FOUND - 404" in {
      wireMockServer.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(notFound.withBody(errorResponse("NOT_FOUND")))
      )

      recoverToExceptionIf[NotFoundException] {
        connector.getOverview(pstr, reportTypeER, fromDt, toDt)
      } map { response =>
        response.responseCode mustEqual NOT_FOUND
        response.message must include("NOT_FOUND")
      }
    }

    "throw Upstream4XX for FORBIDDEN - 403" in {

      wireMockServer.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(forbidden.withBody(errorResponse("FORBIDDEN")))
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.getOverview(pstr, reportTypeER, fromDt, toDt)
      }.map { ex =>
        ex.statusCode mustBe FORBIDDEN
        ex.message must include("FORBIDDEN")
      }
    }

    "throw Upstream5XX for INTERNAL SERVER ERROR - 500" in {

      wireMockServer.stubFor(
        get(urlEqualTo(getErOverviewUrl))
          .willReturn(serverError.withBody(errorResponse("SERVER_ERROR")))
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.getOverview(pstr, reportTypeER, fromDt, toDt)
      }.map { ex =>
        ex.statusCode mustBe INTERNAL_SERVER_ERROR
        ex.message must include("SERVER_ERROR")
      }
    }
  }

  "getEvent" must {
    "API 1832" must {
      "return the json returned from ETMP for valid event" in {
        wireMockServer.stubFor(
          get(urlEqualTo(getApi1832Url))
            .withHeader("reportVersionNumber", equalTo("001"))
            .willReturn(
              ok
                .withHeader("Content-Type", "application/json")
                .withBody(Json.obj("success" -> Json.obj("a" -> "b")).toString())
            )
        )

        connector.getEvent(pstr, fromDt, reportVersion, Some(Event3)).map { response =>
          response mustBe Some(Json.obj("a" -> "b"))
        }
      }

      "return nothing for NOT FOUND - 404" in {
        wireMockServer.stubFor(
          get(urlEqualTo(getApi1832Url))
            .willReturn(notFound.withBody(errorResponse("NOT_FOUND")))
        )

        connector.getEvent(pstr, fromDt, reportVersion, Some(Event3)).map { response =>
          response mustBe None
        }
      }

      "throw Upstream5XX for INTERNAL SERVER ERROR - 500" in {

        wireMockServer.stubFor(
          get(urlEqualTo(getApi1832Url))
            .willReturn(serverError.withBody(errorResponse("SERVER_ERROR")))
        )

        recoverToExceptionIf[UpstreamErrorResponse] {
          connector.getEvent(pstr, fromDt, reportVersion, Some(Event3))
        }.map {
          _.statusCode mustBe INTERNAL_SERVER_ERROR
        }
      }
    }

    "API 1833" must {
      "return the json returned from ETMP for valid event" in {
        wireMockServer.stubFor(
          get(urlEqualTo(getApi1833Url))
            .willReturn(
              ok
                .withHeader("Content-Type", "application/json")
                .withBody(Json.obj("success" -> Json.obj("a" -> "b")).toString())
            )
        )

        connector.getEvent(pstr, fromDt, reportVersion, Some(Event1)).map { response =>
          response mustBe Some(Json.obj("a" -> "b"))
        }
      }

      "return nothing for NOT FOUND - 404" in {
        wireMockServer.stubFor(
          get(urlEqualTo(getApi1833Url))
            .willReturn(notFound.withBody(errorResponse("NOT_FOUND")))
        )

        connector.getEvent(pstr, fromDt, reportVersion, Some(Event1)).map { response =>
          response mustBe None
        }

      }

      "throw Upstream5XX for INTERNAL SERVER ERROR - 500" in {

        wireMockServer.stubFor(
          get(urlEqualTo(getApi1833Url))
            .willReturn(serverError.withBody(errorResponse("SERVER_ERROR")))
        )

        recoverToExceptionIf[UpstreamErrorResponse] {
          connector.getEvent(pstr, fromDt, reportVersion, Some(Event1))
        }.map {
          _.statusCode mustBe INTERNAL_SERVER_ERROR
        }
      }
    }

    "API 1834" must {
      "return the json returned from ETMP for valid event" in {
        wireMockServer.stubFor(
          get(urlEqualTo(getApi1834Url))
            .willReturn(
              ok
                .withHeader("Content-Type", "application/json")
                .withBody(Json.obj("success" -> Json.obj("a" -> "b")).toString())
            )
        )

        connector.getEvent(pstr, fromDt, reportVersion, None).map { response =>
          response mustBe Some(Json.obj("a" -> "b"))
        }
      }

      "return nothing for NOT FOUND - 404" in {
        wireMockServer.stubFor(
          get(urlEqualTo(getApi1834Url))
            .willReturn(notFound.withBody(errorResponse("NOT_FOUND")))
        )

          connector.getEvent(pstr, fromDt, reportVersion, Some(Event10)).map { response =>
            response mustBe None
          }
      }

      "throw Upstream5XX for INTERNAL SERVER ERROR - 500" in {

        wireMockServer.stubFor(
          get(urlEqualTo(getApi1834Url))
            .willReturn(serverError.withBody(errorResponse("SERVER_ERROR")))
        )

        recoverToExceptionIf[UpstreamErrorResponse] {
          connector.getEvent(pstr, fromDt, reportVersion, Some(Event10))
        }.map {
          _.statusCode mustBe INTERNAL_SERVER_ERROR
        }
      }
    }

    "API 1831" must {
      "return the json returned from ETMP for valid event" in {
        wireMockServer.stubFor(
          get(urlEqualTo(getApi1831Url))
            .willReturn(
              ok
                .withHeader("Content-Type", "application/json")
                .withBody(Json.obj("success" -> Json.obj("a" -> "b")).toString())
            )
        )

        connector.getEvent(pstr, fromDt, reportVersion, Some(Event20A)).map { response =>
          response mustBe Some(Json.obj("a" -> "b"))
        }
      }

      "return nothing for NOT FOUND - 404" in {
        wireMockServer.stubFor(
          get(urlEqualTo(getApi1831Url))
            .willReturn(notFound.withBody(errorResponse("NOT_FOUND")))
        )

        connector.getEvent(pstr, fromDt, reportVersion, Some(Event20A)).map { response =>
          response mustBe None
        }
      }

      "throw Upstream5XX for INTERNAL SERVER ERROR - 500" in {

        wireMockServer.stubFor(
          get(urlEqualTo(getApi1831Url))
            .willReturn(serverError.withBody(errorResponse("SERVER_ERROR")))
        )

        recoverToExceptionIf[UpstreamErrorResponse] {
          connector.getEvent(pstr, fromDt, reportVersion, Some(Event20A))
        }.map {
          _.statusCode mustBe INTERNAL_SERVER_ERROR
        }
      }
    }

    "No Get API for this Event Type" must {
      "return None for invalid event" in {
        connector.getEvent(pstr, fromDt, reportVersion, Some(DummyForTest)).map { option =>
          option mustBe None
        }
      }
    }
  }

  "submitEventDeclarationReport" must {
    val data = Json.obj("Id" -> "value")
    "return 200 when ETMP has returned OK & send audit event" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1828Url))
          .withHeader("Content-Type", equalTo("application/json"))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(ok.withBody(Json.obj("success" -> Json.obj("key" -> "value")).toString))
      )

      connector.submitEventDeclarationReport(pstr, data, reportVersion).map {
        verify(mockPostToAPIAuditService, times(1))
          .sendSubmitEventDeclarationAuditEvent(eqTo(pstr), eqTo(data), eqTo(reportVersion), eqTo(None))(any(), any())
        _.status mustBe OK
      }
    }

    "return Upstream5xxResponse when ETMP has returned Internal Server Error and send audit event" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1828Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(serverError())
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.submitEventDeclarationReport(pstr, data, reportVersion)
      }.map {
        verify(mockPostToAPIAuditService, times(1))
          .sendSubmitEventDeclarationAuditEvent(eqTo(pstr), eqTo(data), eqTo(reportVersion), eqTo(None))(any(), any())
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "return 400 when ETMP has returned BadRequestException" in {
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1828Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(badRequest().withBody("INVALID_PAYLOAD"))
      )

      recoverToExceptionIf[BadRequestException] {
        connector.submitEventDeclarationReport(pstr, data, reportVersion)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }
  }

  "return BAD REQUEST when ETMP has returned BadRequestException without Invalid " in {
    val data = Json.obj("success" -> Json.obj("Id" -> "value"))
    wireMockServer.stubFor(
      post(urlEqualTo(postApi1828Url))
        .withRequestBody(equalTo(Json.stringify(data)))
        .willReturn(badRequest())
    )

    recoverToExceptionIf[BadRequestException] {
      connector.submitEventDeclarationReport(pstr, data, reportVersion)
    } map {
      _.responseCode mustEqual BAD_REQUEST
    }
  }

  "return NOT FOUND when ETMP has returned NotFoundException" in {
    val data = Json.obj("success" -> Json.obj("Id" -> "value"))
    wireMockServer.stubFor(
      post(urlEqualTo(postApi1828Url))
        .withRequestBody(equalTo(Json.stringify(data)))
        .willReturn(notFound())
    )

    recoverToExceptionIf[NotFoundException] {
      connector.submitEventDeclarationReport(pstr, data, reportVersion)
    } map {
      _.responseCode mustEqual NOT_FOUND
    }
  }

  "return 4xx when ETMP has returned Upstream error response" in {
    val data = Json.obj(fields = "Id" -> "value")
    wireMockServer.stubFor(
      post(urlEqualTo(postApi1828Url))
        .withRequestBody(equalTo(Json.stringify(data)))
        .willReturn(forbidden())
    )

    recoverToExceptionIf[UpstreamErrorResponse] {
      connector.submitEventDeclarationReport(pstr, data, reportVersion)
    }.map {
      _.statusCode mustBe FORBIDDEN
    }
  }

  "return 204 when ETMP has returned Unrecognized http response" in {
    val data = Json.obj(fields = "Id" -> "value")
    wireMockServer.stubFor(
      post(urlEqualTo(postApi1828Url))
        .withRequestBody(equalTo(Json.stringify(data)))
        .willReturn(noContent())
    )

    recoverToExceptionIf[UnrecognisedHttpResponseException] {
      connector.submitEventDeclarationReport(pstr, data, reportVersion)
    }.map {
      _.getMessage must include("204")
    }
  }

  "getVersions" must {
    "return successfully when DES has returned OK" in {

      wireMockServer.stubFor(
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

      wireMockServer.stubFor(
        get(urlEqualTo(getErVersionUrl(reportTypeER)))
          .willReturn(notFound())
      )

      recoverToExceptionIf[NotFoundException] {
        connector.getVersions(pstr, reportTypeER, "")
      }.map {
        _.responseCode mustBe NOT_FOUND
      }
    }

  }

  "submitEvent20ADeclarationReport" must {

    "return successfully when ETMP has returned OK" in {
      val data = Json.obj("Id" -> "value")
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1829Url))
          .withHeader("Content-Type", equalTo("application/json"))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(ok.withBody(Json.obj("success" -> Json.obj("key" -> "value")).toString))
      )
      connector.submitEvent20ADeclarationReport(pstr, data, reportVersion).map {
        verify(mockPostToAPIAuditService, times(1))
          .sendSubmitEventDeclarationAuditEvent(eqTo(pstr), eqTo(data), eqTo(reportVersion), eqTo(Some(Event20A)))(any(), any())
        _.status mustBe OK
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException" in {
      val data = Json.obj("Id" -> "value")
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1829Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(badRequest().withBody("INVALID_PAYLOAD"))
      )

      recoverToExceptionIf[BadRequestException] {
        connector.submitEvent20ADeclarationReport(pstr, data, reportVersion)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return BAD REQUEST when ETMP has returned BadRequestException without Invalid " in {
      val data = Json.obj("Id" -> "value")
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1829Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(badRequest())
      )

      recoverToExceptionIf[BadRequestException] {
        connector.submitEvent20ADeclarationReport(pstr, data, reportVersion)
      } map {
        _.responseCode mustEqual BAD_REQUEST
      }
    }

    "return NOT FOUND when ETMP has returned NotFoundException" in {
      val data = Json.obj("Id" -> "value")
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1829Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(notFound())
      )

      recoverToExceptionIf[NotFoundException] {
        connector.submitEvent20ADeclarationReport(pstr, data, reportVersion)
      } map {
        _.responseCode mustEqual NOT_FOUND
      }
    }

    "return Upstream5xxResponse when ETMP has returned Internal Server Error" in {
      val data = Json.obj("Id" -> "value")
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1829Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(serverError())
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.submitEvent20ADeclarationReport(pstr, data, reportVersion)
      }.map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "return 4xx when ETMP has returned Upstream error response" in {
      val data = Json.obj("Id" -> "value")
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1829Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(forbidden())
      )

      recoverToExceptionIf[UpstreamErrorResponse] {
        connector.submitEvent20ADeclarationReport(pstr, data, reportVersion)
      }.map {
        _.statusCode mustBe FORBIDDEN
      }
    }

    "return 204 when ETMP has returned Unrecognized http response" in {
      val data = Json.obj("Id" -> "value")
      wireMockServer.stubFor(
        post(urlEqualTo(postApi1829Url))
          .withRequestBody(equalTo(Json.stringify(data)))
          .willReturn(noContent())
      )

      recoverToExceptionIf[UnrecognisedHttpResponseException] {
        connector.submitEvent20ADeclarationReport(pstr, data, reportVersion)
      }.map {
        _.getMessage must include("204")
      }
    }
  }

}

object HipEventReportConnectorSpec {
  private val psaId: String = "testpsa"
  private val pstr: String = "test-pstr"
  private val reportVersion: String = "1"
  private val reportTypeER: String = "ER"
  private val fromDt: String = "2022-04-06"
  private val toDt: String = "2023-04-05"
  private val startDt: String = "2022-04-01"

  private val postApi1826Url: String =
    s"/RESTAdapter/pension-online/event-reports/pods/$pstr"
  private val postApi1827Url: String =
    s"/RESTAdapter/pension-online/event1-reports/pods/$pstr"
  private val postApi1828Url: String =
    s"/RESTAdapter/pension-online/event-declaration-reports/$pstr"
  private val postApi1829Url: String =
    s"/RESTAdapter/pension-online/event20a-declaration-reports/$pstr"
  private val postApi1830Url: String =
    s"/RESTAdapter/pension-online/member-event-reports/$pstr"
  private val getApi1831Url: String =
    s"/RESTAdapter/pension-online/event20a-status-reports/$pstr"
  private val getApi1832Url: String =
    s"/RESTAdapter/pension-online/member-event-status-reports/$pstr"
  private val getApi1833Url: String =
    s"/RESTAdapter/pension-online/event1-status-reports/$pstr"
  private val getApi1834Url: String =
    s"/RESTAdapter/pension-online/event-status-reports/$pstr"
  private val getErOverviewUrl: String =
    s"/pension-online/reports/overview/pods/$pstr/ER?fromDate=$fromDt&toDate=$toDt"
  private def getErVersionUrl(reportType: String): String =
    s"/pension-online/reports/$pstr/$reportType/versions?startDate=$startDt"

  private val overview1: EROverview =
    EROverview(
      periodStartDate   = LocalDate.of(2022, 4, 6),
      periodEndDate     = LocalDate.of(2023, 4, 5),
      tpssReportPresent = false,
      versionDetails    = Some(EROverviewVersion(
        numberOfVersions          = 3,
        submittedVersionAvailable = false,
        compiledVersionAvailable  = true
      ))
    )

  private val overview2: EROverview =
    EROverview(
      periodStartDate   = LocalDate.of(2022, 4, 6),
      periodEndDate     = LocalDate.of(2023, 4, 5),
      tpssReportPresent = false,
      versionDetails    = Some(EROverviewVersion(
        numberOfVersions          = 2,
        submittedVersionAvailable = true,
        compiledVersionAvailable  = true
      ))
    )

  private val erVersionResponseJson: JsArray =
    Json.arr(
      Json.obj(
        "reportFormBundleNumber" -> "123456789012",
        "reportVersion" -> 1,
        "reportStatus" -> "Compiled",
        "compilationOrSubmissionDate" -> s"${startDt}T09:30:47Z",
        "reportSubmitterDetails" -> Json.obj(
          "reportSubmittedBy" -> "PSP",
          "organisationOrPartnershipDetails" -> Json.obj(
            "organisationOrPartnershipName" -> "ABC Limited"
          )
        ),
        "psaDetails" -> Json.obj(
          "psaOrganisationOrPartnershipDetails" -> Json.obj(
            "organisationOrPartnershipName" -> "XYZ Limited"
          )
        )
      )
    )

  private val erVersions: JsValue =
    Json.parse(
      """[
          {
            "reportFormBundleNumber": "123456789012",
            "reportVersion": 1,
            "reportStatus": "Compiled",
            "compilationOrSubmissionDate": "2022-04-01T09:30:47Z",
            "reportSubmitterDetails": {
              "reportSubmittedBy": "PSP",
              "organisationOrPartnershipDetails": {
              "organisationOrPartnershipName": "ABC Limited"
            }
            },
            "psaDetails": {
              "psaOrganisationOrPartnershipDetails": {
              "organisationOrPartnershipName": "XYZ Limited"
            }
            }
          }
      ]""".stripMargin
    )
}

