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

package controllers

import connectors.EventReportConnector
import models.{EROverview, EROverviewVersion, ERVersion}
import org.mockito.ArgumentMatchers.any
import org.mockito.{ArgumentMatchers, MockitoSugar}
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.json._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http._
import utils.{ErrorReport, JSONPayloadSchemaValidator}

import java.time.LocalDate
import scala.concurrent.Future

class EventReportControllerSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfter {

  import EventReportControllerSpec._

  implicit val hc: HeaderCarrier = HeaderCarrier()
  private val fakeRequest = FakeRequest("GET", "/")
  private val mockEventReportConnector = mock[EventReportConnector]
  private val mockJSONPayloadSchemaValidator = mock[JSONPayloadSchemaValidator]
  private val authConnector: AuthConnector = mock[AuthConnector]
  val modules: Seq[GuiceableModule] =
    Seq(
      bind[AuthConnector].toInstance(authConnector),
      bind[EventReportConnector].toInstance(mockEventReportConnector),
      bind[JSONPayloadSchemaValidator].toInstance(mockJSONPayloadSchemaValidator)
    )

  val application: Application = new GuiceApplicationBuilder()
    .configure(conf = "auditing.enabled" -> false, "metrics.enabled" -> false, "metrics.jvm" -> false).
    overrides(modules: _*).build()

  before {
    reset(mockEventReportConnector, authConnector)
    when(authConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(Some("Ext-137d03b9-d807-4283-a254-fb6c30aceef1"))
    when(mockJSONPayloadSchemaValidator.validateJsonPayload(any(), any())) thenReturn Right(true)
  }

  "compileEventReportSummary" must {
    "return OK when valid response" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportConnector.compileEventReportSummary(any(), any())(any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, compileEventReportSummaryResponseJson.toString)))

      val result = controller.compileEventReportSummary(fakeRequest.withJsonBody(compileEventReportSummaryResponseJson).withHeaders(
        newHeaders = "pstr" -> pstr))
      status(result) mustBe OK
    }

    "return 400 when validation errors response" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportConnector.compileEventReportSummary(any(), any())(any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, compileEventReportSummaryResponseJson.toString)))

      val listErrors: List[ErrorReport] = List(
        ErrorReport("instance1", "errors1"),
        ErrorReport("instance2", "errors2")
      )

      when(mockJSONPayloadSchemaValidator.validateJsonPayload(any(), any())) thenReturn Left(listErrors)

      recoverToExceptionIf[EventReportValidationFailureException] {
        controller.compileEventReportSummary(fakeRequest.withJsonBody(compileEventReportSummaryResponseJson).withHeaders(
          newHeaders = "pstr" -> pstr))
      } map {
        failure =>
          failure.exMessage mustBe "Schema validation errors:-\n(instance1: errors1),\n(instance2: errors2)"
      }
    }

    "throw Upstream5XXResponse on Internal Server Error" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportConnector.compileEventReportSummary(any(), any())(any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))
      recoverToExceptionIf[UpstreamErrorResponse] {
        controller.compileEventReportSummary(fakeRequest.withJsonBody(compileEventReportSummaryResponseJson).
          withHeaders(newHeaders = "pstr" -> pstr))
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "throw BadRequestException when request body not provided" in {

      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.compileEventReportSummary()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include(s"Bad Request without pstr (Some($pstr)) or request body (None)")
      }
    }

    "throw BadRequestException when PSTR missing in header" in {

      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.compileEventReportSummary(fakeRequest.withJsonBody(compileEventReportSummaryResponseJson))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include(s"Bad Request without pstr (None) or request body (Some($compileEventReportSummaryResponseJson))")
      }
    }

    "throw Unauthorized exception if auth fails" in {
      when(authConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[UnauthorizedException] {
        controller.compileEventReportSummary()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }

  }

  "getOverview" must {

    "return OK with the Seq of overview details" in {
      when(mockEventReportConnector.getOverview(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(reportTypeER),
        ArgumentMatchers.eq(startDt),
        ArgumentMatchers.eq(endDt))(any(), any()))
        .thenReturn(Future.successful(erOverview))

      val controller = application.injector.instanceOf[EventReportController]
      val result = controller.getOverview(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "reportType" -> "ER", "startDate" -> startDt, "endDate" -> endDt))

      status(result) mustBe OK
      contentAsJson(result) mustBe erOverviewResponseJson
    }

    "throw a Bad Request Exception when endDate parameter is missing in header" in {
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "startDate" -> "2022-04-06", "reportType" -> "er"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Bad Request with missing parameters: end date missing")
      }
    }

    "throw a Bad Request Exception when startDate parameter is missing in header" in {
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "endDate" -> "2022-04-06", "reportType" -> "er"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Bad Request with missing parameters: start date missing")
      }
    }

    "throw a Bad Request Exception when pstr parameter is missing in header" in {
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "startDate" -> "2022-04-06", "endDate" -> "2022-04-06", "reportType" -> "er"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Bad Request with missing parameters: PSTR missing")
      }
    }

    "throw a Bad Request Exception when reportType parameter is missing in header" in {
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "startDate" -> "2022-04-06", "endDate" -> "2022-04-06"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Bad Request with missing parameters: report type missing")
      }
    }

    "throw a Bad Request Exception when all required parameters are missing in header" in {
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest)
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Bad Request with missing parameters: PSTR missing report type missing start date missing end date missing")
      }
    }

    "throw a Unauthorised Exception if auth fails" in {
      when(authConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[UnauthorizedException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "fromDate" -> "2021-04-06", "toDate" -> "2022-04-05"))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "compileEventReportOne" must {
    "return OK when valid response" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportConnector.compileEventOneReport(any(), any())(any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, compileEventOneReportSuccessResponse.toString)))

      val result = controller.compileEventOneReport(fakeRequest.withJsonBody(compileEventOneReportSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr))

      status(result) mustBe OK
    }

    "return OK when validation errors response" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportConnector.compileEventOneReport(any(), any())(any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, compileEventOneReportSuccessResponse.toString)))

      val listErrors: List[ErrorReport] = List(
        ErrorReport("instance1", "errors1"),
        ErrorReport("instance2", "errors2")
      )

      when(mockJSONPayloadSchemaValidator.validateJsonPayload(any(), any())) thenReturn Left(listErrors)


      recoverToExceptionIf[EventReportValidationFailureException] {
        controller.compileEventOneReport(fakeRequest.withJsonBody(compileEventOneReportSuccessResponse).withHeaders(
          newHeaders = "pstr" -> pstr))
      } map {
        failure =>
          failure.exMessage mustBe "Schema validation errors:-\n(instance1: errors1),\n(instance2: errors2)"
      }
    }

    "throw Upstream5XXResponse on Internal Server Error" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportConnector.compileEventOneReport(any(), any())(any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))
      recoverToExceptionIf[UpstreamErrorResponse] {
        controller.compileEventOneReport(fakeRequest.withJsonBody(compileEventOneReportSuccessResponse).
          withHeaders(newHeaders = "pstr" -> pstr))
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "throw BadRequestException when request body not provided" in {

      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.compileEventReportSummary()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include(s"Bad Request without pstr (Some($pstr)) or request body (None)")
      }
    }

    "throw BadRequestException when PSTR missing in header" in {

      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.compileEventReportSummary(fakeRequest.withJsonBody(compileEventOneReportSuccessResponse))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include(s"Bad Request without pstr (None) or request body (Some($compileEventOneReportSuccessResponse))")
      }
    }

    "throw Unauthorized exception if auth fails" in {
      when(authConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[UnauthorizedException] {
        controller.compileEventOneReport()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "submitEventDeclarationReport" must {
    "return OK when valid response" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportConnector.submitEventDeclarationReport(any(), any())(any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, submitEventDeclarationReportSuccessResponse.toString)))

      val result = controller.submitEventDeclarationReport(fakeRequest.withJsonBody(submitEventDeclarationReportSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr))

      status(result) mustBe OK
    }

    "throw Upstream5XXResponse on Internal Server Error" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportConnector.submitEventDeclarationReport(any(), any())(any(), any()))
        .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))
      recoverToExceptionIf[UpstreamErrorResponse] {
        controller.submitEventDeclarationReport(fakeRequest.withJsonBody(submitEventDeclarationReportSuccessResponse).
          withHeaders(newHeaders = "pstr" -> pstr))
      } map {
        _.statusCode mustBe INTERNAL_SERVER_ERROR
      }
    }

    "throw BadRequestException when request body not provided" in {

      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.submitEventDeclarationReport()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include(s"Bad Request without pstr (Some($pstr)) or request body (None)")
      }
    }

    "throw BadRequestException when PSTR missing in header" in {

      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.submitEventDeclarationReport(fakeRequest.withJsonBody(submitEventDeclarationReportSuccessResponse))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include(s"Bad Request without pstr (None) or request body (Some($submitEventDeclarationReportSuccessResponse))")
      }
    }

    "throw Unauthorized exception if auth fails" in {
      when(authConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[UnauthorizedException] {
        controller.submitEventDeclarationReport()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "getVersions" must {
    "return OK with the Seq of Version" in {
      when(mockEventReportConnector.getVersions(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(reportTypeER),
        ArgumentMatchers.eq(startDt))(any(), any()))
        .thenReturn(Future.successful(erVersions))

      val controller = application.injector.instanceOf[EventReportController]
      val result = controller.getVersions(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "reportType" -> "ER", "startDate" -> startDt))

      status(result) mustBe OK
      contentAsJson(result) mustBe erVersionResponseJson
    }

    "throw a Bad Request Exception when startDt parameter is missing in header" in {
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.getVersions()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "reportType" -> "ER"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include("Bad Request for version with missing parameters:   start date missing ")
      }
    }
    "throw a Unauthorised Exception if auth fails" in {
      when(authConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[UnauthorizedException] {
        controller.getVersions()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "reportType" -> "ER", "startDate" -> "2021-04-06"))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }
}

object EventReportControllerSpec {
  val pstr = "pstr"

  val compileEventReportSummaryResponseJson: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678912")
  val compileEventOneReportSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678988")

  private val startDt = "2022-04-06"
  private val endDt = "2023-04-05"
  private val reportTypeER = "ER"

  val erOverviewResponseJson: JsArray = Json.arr(
    Json.obj(
      "periodStartDate" -> "2022-04-06",
      "periodEndDate" -> "2023-04-05",
      "tpssReportPresent" -> false,
      "versionDetails" -> Json.obj(
        "numberOfVersions" -> 3,
        "submittedVersionAvailable" -> false,
        "compiledVersionAvailable" -> true
      )),
    Json.obj(
      "periodStartDate" -> "2022-04-06",
      "periodEndDate" -> "2023-04-05",
      "tpssReportPresent" -> false,
      "versionDetails" -> Json.obj(
        "numberOfVersions" -> 2,
        "submittedVersionAvailable" -> true,
        "compiledVersionAvailable" -> true
      )),
  )

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

  val submitEventDeclarationReportSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678933")

  private val erVersionResponseJson: JsArray = Json.arr(
    Json.obj(
      "reportVersion" -> 1,
      "reportStatus" -> "Compiled",
      "date" -> startDt
      )
  )

  private val version = ERVersion( 1,
    LocalDate.of(2022, 4, 6),
    "Compiled")
  private val erVersions = Seq(version)
}

