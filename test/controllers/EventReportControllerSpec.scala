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
import connectors.cache.OverviewCacheConnector
import models.{EROverview, EROverviewVersion, ERVersion}
import org.mockito.ArgumentMatchers.any
import org.mockito.{ArgumentMatchers, MockitoSugar}
import org.scalatest.BeforeAndAfter
import org.scalatest.concurrent.ScalaFutures.whenReady
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.json._
import play.api.mvc.Results.NoContent
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.EventReportCacheRepository
import services.EventReportService
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
  private val mockEventReportCacheRepository = mock[EventReportCacheRepository]
  private val mockEventReportService = mock[EventReportService]
  private val mockAuthConnector: AuthConnector = mock[AuthConnector]
  private val mockOverviewCacheConnector = mock[OverviewCacheConnector]

  val modules: Seq[GuiceableModule] =
    Seq(
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[EventReportConnector].toInstance(mockEventReportConnector),
      bind[JSONPayloadSchemaValidator].toInstance(mockJSONPayloadSchemaValidator),
      bind[EventReportCacheRepository].toInstance(mockEventReportCacheRepository),
      bind[EventReportService].toInstance(mockEventReportService),
      bind[OverviewCacheConnector].toInstance(mockOverviewCacheConnector)
    )

  val application: Application = new GuiceApplicationBuilder()
    .configure(conf = "auditing.enabled" -> false, "metrics.enabled" -> false, "metrics.jvm" -> false).
    overrides(modules: _*).build()

  before {
    reset(mockEventReportConnector, mockAuthConnector, mockOverviewCacheConnector, mockJSONPayloadSchemaValidator)
    when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(Some("Ext-137d03b9-d807-4283-a254-fb6c30aceef1"))
    when(mockJSONPayloadSchemaValidator.validateJsonPayload(any(), any())) thenReturn Right(true)
  }

  "getOverview" must {

    "return OK with the Seq of overview details and save the data in cache if no data was found in the cache to begin with" in {
      when(mockEventReportConnector.getOverview(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(reportTypeER),
        ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(endDate))(any(), any()))
        .thenReturn(Future.successful(erOverview))
      when(mockOverviewCacheConnector.get(any(), any(), any(), any())(any())).thenReturn(Future.successful(None))
      when(mockOverviewCacheConnector.save(any(), any(), any(), any(), any())(any())).thenReturn(Future.successful(true))
      val controller = application.injector.instanceOf[EventReportController]
      val result = controller.getOverview(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "reportType" -> "ER", "startDate" -> startDate, "endDate" -> endDate))

      whenReady(result) { _ =>
        verify(mockOverviewCacheConnector, times(1)).get(any(), any(), any(), any())(any())
        verify(mockOverviewCacheConnector, times(1)).save(any(), any(), any(), any(), any())(any())
        verify(mockEventReportConnector, times(1)).getOverview(any(), any(), any(), any())(any(), any())

        status(result) mustBe OK
        contentAsJson(result) mustBe erOverviewResponseJson
      }
    }

    "return OK with the Seq of overview details and don't try to save the data in cache if the data already exists in the cache" in {
      when(mockOverviewCacheConnector.get(any(), any(), any(), any())(any())).thenReturn(Future.successful(Some(Json.toJson(erOverview))))
      val controller = application.injector.instanceOf[EventReportController]
      val result = controller.getOverview(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "reportType" -> "ER", "startDate" -> startDate, "endDate" -> endDate))

      whenReady(result) { _ =>
        verify(mockOverviewCacheConnector, times(1)).get(any(), any(), any(), any())(any())
        verify(mockOverviewCacheConnector, never).save(any(), any(), any(), any(), any())(any())
        verify(mockEventReportConnector, never).getOverview(any(), any(), any(), any())(any(), any())
        status(result) mustBe OK
        contentAsJson(result) mustBe erOverviewResponseJson
      }
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
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[UnauthorizedException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "fromDate" -> "2021-04-06", "toDate" -> "2022-04-05"))
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

    "return OK when validation errors response" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportConnector.submitEventDeclarationReport(any(), any())(any(), any()))
        .thenReturn(Future.successful(HttpResponse(OK, submitEventDeclarationReportSuccessResponse.toString)))

      val listErrors: List[ErrorReport] = List(
        ErrorReport("instance1", "errors1"),
        ErrorReport("instance2", "errors2")
      )

      when(mockJSONPayloadSchemaValidator.validateJsonPayload(any(), any())) thenReturn Left(listErrors)


      recoverToExceptionIf[EventReportValidationFailureException] {
        controller.submitEventDeclarationReport(fakeRequest.withJsonBody(submitEventDeclarationReportSuccessResponse).withHeaders(
          newHeaders = "pstr" -> pstr))
      } map {
        failure =>
          failure.exMessage mustBe "Schema validation errors:-\n(instance1: errors1),\n(instance2: errors2)"
      }
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
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)
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
        ArgumentMatchers.eq(startDate))(any(), any()))
        .thenReturn(Future.successful(erVersions))

      val controller = application.injector.instanceOf[EventReportController]
      val result = controller.getVersions(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "reportType" -> "ER", "startDate" -> startDate))

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
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[UnauthorizedException] {
        controller.getVersions()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "reportType" -> "ER", "startDate" -> "2021-04-06"))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "saveEvent" must {
    "return 201 Created when valid response" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportCacheRepository.upsert(any(), any(), any())(any()))
        .thenReturn(Future.successful(HttpResponse(OK, saveEventSuccessResponse.toString)))

      val result = controller.saveEvent(fakeRequest.withJsonBody(saveEventSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr, "eventType" -> eventType))

      status(result) mustBe CREATED
    }

    "throw a 400 Bad Request Exception when eventType parameter not in enum" in {
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.saveEvent()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "eventType" -> badRequestEventType).withJsonBody(saveEventSuccessResponse))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include(s"Bad Request: invalid eventType ($badRequestEventType)")
      }
    }

    "throw a 400 Bad Request Exception when eventType missing" in {
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.saveEvent()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr).withJsonBody(saveEventSuccessResponse))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include(s"Bad Request without pstr (Some($pstr)) or eventType (None) or request body (Some($saveEventSuccessResponse))")
      }
    }

    "throw a 401 Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[UnauthorizedException] {
        controller.saveEvent()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "eventType" -> eventType).withJsonBody(saveEventSuccessResponse))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "compileEvent" must {
    "return 204 No Content when valid response" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportService.compileEventReport(any(), any())(any(), any()))
        .thenReturn(Future.successful(NoContent))

      val result = controller.compileEvent(fakeRequest.withJsonBody(compileEventSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr))

      status(result) mustBe NO_CONTENT
    }

    "throw a 401 Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)
      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[UnauthorizedException] {
        controller.compileEvent()(fakeRequest.withJsonBody(compileEventSuccessResponse).withHeaders(newHeaders = "pstr" -> pstr))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }
}

object EventReportControllerSpec {
  val pstr = "pstr"

  val eventType = "1"
  val badRequestEventType = "666"
  val badRequestBody: JsObject = Json.obj("Testing" -> 123456789)

  val compileEventReportSummaryResponseJson: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678912")
  val compileEventOneReportSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678988")

  private val startDate = "2022-04-06"
  private val endDate = "2023-04-05"
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
      "date" -> startDate
    )
  )

  private val version = ERVersion(1,
    LocalDate.of(2022, 4, 6),
    "Compiled")
  private val erVersions = Seq(version)

  val saveEventSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678955")

  val compileEventSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678977")
}

