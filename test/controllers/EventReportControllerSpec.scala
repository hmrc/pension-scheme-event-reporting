/*
 * Copyright 2023 HM Revenue & Customs
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

import models.enumeration.EventType._
import models.{ERVersion, EventReportValidationFailureException}
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{never, reset, verify, when}
import org.scalatest.BeforeAndAfter
import org.scalatest.concurrent.ScalaFutures.whenReady
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.json._
import play.api.mvc.Results.NoContent
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.EventReportService
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.http._
import utils.JSONSchemaValidator

import java.time.LocalDate
import scala.concurrent.Future
import scala.util.{Failure, Success}

class EventReportControllerSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfter {

  import EventReportControllerSpec._

  implicit val hc: HeaderCarrier = HeaderCarrier()
  private val fakeRequest = FakeRequest("GET", "/")
  private val mockJSONPayloadSchemaValidator = mock[JSONSchemaValidator]
  private val mockEventReportService = mock[EventReportService]
  private val mockAuthConnector: AuthConnector = mock[AuthConnector]

  val modules: Seq[GuiceableModule] =
    Seq(
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[JSONSchemaValidator].toInstance(mockJSONPayloadSchemaValidator),
      bind[EventReportService].toInstance(mockEventReportService)
    )

  val application: Application = new GuiceApplicationBuilder()
    .configure(conf = "auditing.enabled" -> false, "metrics.enabled" -> false, "metrics.jvm" -> false).
    overrides(modules: _*).build()
  private val controller = application.injector.instanceOf[EventReportController]

  before {
    reset(mockAuthConnector)
    reset(mockJSONPayloadSchemaValidator)
    reset(mockEventReportService)
    when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(Some("Ext-137d03b9-d807-4283-a254-fb6c30aceef1"))
    when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any())).thenReturn(Failure(EventReportValidationFailureException("Test")))
  }

  "getOverview" must {
    "return OK with the overview payload returned from service" in {
      when(mockEventReportService.getOverview(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(reportTypeER),
        ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(endDate))(any(), any()))
        .thenReturn(Future.successful(erOverviewResponseJson))

      val result = controller.getOverview(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "reportType" -> "ER", "startDate" -> startDate, "endDate" -> endDate))

      whenReady(result) { _ =>
        status(result) mustBe OK
        contentAsJson(result) mustBe erOverviewResponseJson
      }
    }


    "throw a Bad Request Exception when endDate parameter is missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "startDate" -> "2022-04-06", "reportType" -> "er"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Bad Request with missing parameters: end date missing")
      }
    }

    "throw a Bad Request Exception when startDate parameter is missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "endDate" -> "2022-04-06", "reportType" -> "er"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Bad Request with missing parameters: start date missing")
      }
    }

    "throw a Bad Request Exception when pstr parameter is missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "startDate" -> "2022-04-06", "endDate" -> "2022-04-06", "reportType" -> "er"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Bad Request with missing parameters: PSTR missing")
      }
    }

    "throw a Bad Request Exception when reportType parameter is missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "startDate" -> "2022-04-06", "endDate" -> "2022-04-06"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Bad Request with missing parameters: report type missing")
      }
    }

    "throw a Bad Request Exception when all required parameters are missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest)
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Bad Request with missing parameters: PSTR missing report type missing start date missing end date missing")
      }
    }

    "throw a Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)

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
      when(mockEventReportService.submitEventDeclarationReport(any(), any())(any(), any()))
        .thenReturn(Future.successful(submitEventDeclarationReportSuccessResponse))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any()))
        .thenReturn(Success(()))

      val result = controller.submitEventDeclarationReport(fakeRequest.withJsonBody(submitEventDeclarationReportSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr))

      status(result) mustBe OK
    }

    "throw validation exception when validation errors response" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any()))
        .thenReturn(Failure(EventReportValidationFailureException("Test")))

      recoverToExceptionIf[EventReportValidationFailureException] {
        controller.submitEventDeclarationReport(fakeRequest.withJsonBody(submitEventDeclarationReportSuccessResponse).withHeaders(
          newHeaders = "pstr" -> pstr))
      } map {
        failure =>
          failure.getMessage mustBe "Test"
      }
    }
  }

  "submitEvent20ADeclarationReport" must {
    "return OK when valid response" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportService.submitEvent20ADeclarationReport(any(), any())(any(), any()))
        .thenReturn(Future.successful(submitEvent20ADeclarationReportSuccessResponse))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any()))
        .thenReturn(Success(()))

      val result = controller.submitEvent20ADeclarationReport(fakeRequest.withJsonBody(submitEvent20ADeclarationReportSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr))

      status(result) mustBe OK
    }

    "throw validation exception when validation errors response" in {
      val controller = application.injector.instanceOf[EventReportController]

      when(mockEventReportService.submitEvent20ADeclarationReport(any(), any())(any(), any()))
        .thenReturn(Future.successful(submitEvent20ADeclarationReportSuccessResponse))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any()))
        .thenReturn(Failure(EventReportValidationFailureException("Test")))

      recoverToExceptionIf[EventReportValidationFailureException] {
        controller.submitEvent20ADeclarationReport(fakeRequest.withJsonBody(submitEvent20ADeclarationReportSuccessResponse).withHeaders(
          newHeaders = "pstr" -> pstr))
      } map {
        failure =>
          failure.getMessage mustBe "Test"
      }
    }
  }


  "getVersions" must {
    "return OK with the Seq of Version" in {
      when(mockEventReportService.getVersions(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(reportTypeER),
        ArgumentMatchers.eq(startDate))(any(), any()))
        .thenReturn(Future.successful(erVersions))

      val result = controller.getVersions(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "reportType" -> "ER", "startDate" -> startDate))

      status(result) mustBe OK
      contentAsJson(result) mustBe erVersionResponseJson
    }

    "throw a Bad Request Exception when startDt parameter is missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getVersions()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "reportType" -> "ER"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include("Bad Request for version with missing parameters:   start date missing ")
      }
    }
    "throw a Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)

      recoverToExceptionIf[UnauthorizedException] {
        controller.getVersions()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "reportType" -> "ER", "startDate" -> "2021-04-06"))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "getEvent" must {
    "return NotFound where no data" in {
      when(mockEventReportService.getEvent(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(versionString),
        ArgumentMatchers.eq(Event1)
      )(any(), any()))
        .thenReturn(Future.successful(None))

      val result = controller.getEvent(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr,
        "startDate" -> startDate,
        "version" -> "1",
        "eventType" -> Event1.toString
      ))

      status(result) mustBe NOT_FOUND
    }

    "return OK for Event 1 with dummy json response" in {
      when(mockEventReportService.getEvent(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(versionString),
        ArgumentMatchers.eq(Event1)
      )(any(), any()))
        .thenReturn(Future.successful(Some(dummyJsValue)))

      val result = controller.getEvent(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr,
        "startDate" -> startDate,
        "version" -> "1",
        "eventType" -> Event1.toString
      ))

      status(result) mustBe OK
      contentAsJson(result) mustBe dummyJsValue
    }

    "return OK for Event 2 with dummy json response" in {
      when(mockEventReportService.getEvent(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(versionString),
        ArgumentMatchers.eq(Event2)
      )(any(), any()))
        .thenReturn(Future.successful(Some(dummyJsValue)))

      val result = controller.getEvent(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr,
        "startDate" -> startDate,
        "version" -> "1",
        "eventType" -> Event2.toString
      ))

      status(result) mustBe OK
      contentAsJson(result) mustBe dummyJsValue
    }

    "throw a Bad Request Exception for an invalid event type" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getEvent(fakeRequest.withHeaders(
          newHeaders = "pstr" -> pstr,
          "startDate" -> startDate,
          "version" -> versionString,
          "eventType" -> invalidEventType
        ))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include(s"Bad Request: invalid eventType ($invalidEventType)")
      }
    }

    "throw a Bad Request Exception when all parameters missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getEvent(fakeRequest.withHeaders(
        ))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include("Bad Request with missing parameters: PSTR missing  event type missing  start date missing  version missing ")
      }
    }

    "throw a Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)

      recoverToExceptionIf[UnauthorizedException] {
        controller.getEvent(fakeRequest.withHeaders(
          newHeaders = "pstr" -> pstr,
          "startDate" -> startDate,
          "version" -> versionString,
          "eventType" -> "Event2"
        ))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "getEventSummary" must {
    "return OK with dummy json response" in {
      when(mockEventReportService.getEventSummary(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(versionString),
        ArgumentMatchers.eq(startDate)
      )(any(), any()))
        .thenReturn(Future.successful(dummyJsValue))

      val result = controller.getEventSummary(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr,
        "reportStartDate" -> startDate,
        "reportVersionNumber" -> versionString
      ))

      status(result) mustBe OK
      contentAsJson(result) mustBe dummyJsValue
    }

    "throw a Bad Request Exception when all parameters missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getEventSummary(fakeRequest)
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include("Bad Request with missing parameters: PSTR missing version missing start date missing")
      }
    }

    "throw a Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)

      recoverToExceptionIf[UnauthorizedException] {
        controller.getEventSummary(fakeRequest)
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "getUserAnswers" must {
    "return 200 OK when valid response" in {

      when(mockEventReportService.getUserAnswers(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(Event1)
      )(any()))
        .thenReturn(Future.successful(Some(json)))

      val result = controller.getUserAnswers(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "eventType" -> eventType))

      status(result) mustBe OK
    }

    "return not found exception when invalid event type" in {

      recoverToExceptionIf[NotFoundException] {
        controller.getUserAnswers(fakeRequest.withHeaders(
          newHeaders = "pstr" -> pstr, "eventType" -> "test"))
      } map { response =>
        verify(mockEventReportService, never).getUserAnswers(any(), any())(any())
        response.responseCode mustBe NOT_FOUND
        response.message must include("Bad Request: eventType (test) not found")
      }
    }

    "return none when not found" in {

      when(mockEventReportService.getUserAnswers(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(Event1)
      )(any()))
        .thenReturn(Future.successful(None))

      val result = controller.getUserAnswers(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "eventType" -> eventType))

      status(result) mustBe NOT_FOUND

    }

    "throw a OK when eventType missing" in {

      when(mockEventReportService.getUserAnswers(
        ArgumentMatchers.eq(pstr)
      )(any()))
        .thenReturn(Future.successful(Some(json)))

      val result = controller.getUserAnswers(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr))

      status(result) mustBe OK
    }

    "throw a 401 Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)

      recoverToExceptionIf[UnauthorizedException] {
        controller.getUserAnswers(fakeRequest.withHeaders(
          newHeaders = "pstr" -> pstr, "eventType" -> eventType))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "saveUserAnswersToCache" must {
    "return 200 OK when valid response" in {

      when(mockEventReportService.saveUserAnswers(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(Event1),
        any()
      )(any()))
        .thenReturn(Future.successful(()))

      val result = controller.saveUserAnswers(fakeRequest.withJsonBody(saveUserAnswersToCacheSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr, "eventType" -> eventType))

      status(result) mustBe OK
    }

    "return not found exception when invalid event type" in {

      recoverToExceptionIf[NotFoundException] {
        controller.saveUserAnswers(fakeRequest.withJsonBody(saveUserAnswersToCacheSuccessResponse).withHeaders(
          newHeaders = "pstr" -> pstr, "eventType" -> "test"))
      } map { response =>
        verify(mockEventReportService, never).saveUserAnswers(any(), any(), any())(any())
        response.responseCode mustBe NOT_FOUND
        response.message must include("Bad Request: eventType (test) not found")
      }
    }

    "return OK when eventType missing" in {
      when(mockEventReportService.saveUserAnswers(
        ArgumentMatchers.eq(pstr),
        any()
      )(any()))
        .thenReturn(Future.successful(()))

      val result = controller.saveUserAnswers(fakeRequest.withJsonBody(saveUserAnswersToCacheSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr))

      status(result) mustBe OK
    }

    "throw a 401 Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)

      recoverToExceptionIf[UnauthorizedException] {
        controller.saveUserAnswers()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "eventType" -> eventType)
          .withJsonBody(saveUserAnswersToCacheSuccessResponse))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "compileEvent" must {
    "return 204 No Content when valid response" in {
      when(mockEventReportService.compileEventReport(any(), any())(any(), any()))
        .thenReturn(Future.successful(NoContent))

      val result = controller.compileEvent(fakeRequest.withJsonBody(compileEventSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr, "eventType" -> "1"))

      status(result) mustBe NO_CONTENT
    }

    "throw a 401 Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)

      recoverToExceptionIf[UnauthorizedException] {
        controller.compileEvent()(fakeRequest.withJsonBody(compileEventSuccessResponse)
          .withHeaders(newHeaders = "pstr" -> pstr, "eventType" -> "1"))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }
}

object EventReportControllerSpec {
  private val dummyJsValue = JsArray(Seq(JsString("test")))
  private val pstr = "pstr"

  private val versionString = "001"
  private val eventType = "1"

  private val startDate = "2022-04-06"
  private val endDate = "2023-04-05"
  private val reportTypeER = "ER"
  private val invalidEventType = "invalidEventType"

  private val erOverviewResponseJson: JsArray = Json.arr(
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

  private val submitEventDeclarationReportSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678933")

  private val submitEvent20ADeclarationReportSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345670811")

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

  private val saveUserAnswersToCacheSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678955")

  private val compileEventSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678977")

  private val json = Json.obj("test" -> "test")
}

