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

package controllers

import com.mongodb.client.result.UpdateResult
import models.enumeration.EventType
import models.enumeration.EventType._
import models.{EventReportValidationFailureException, UserLockedException}
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.{any, booleanThat}
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
import play.api.mvc.Results.{BadRequest, NoContent}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.EventReportService
import uk.gov.hmrc.auth.core.retrieve.{Name, ~}
import uk.gov.hmrc.auth.core.{AuthConnector, Enrolment, EnrolmentIdentifier, Enrolments}
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

  private val emptyEnrolments = Enrolments(Set(): Set[Enrolment])

  private val enrolments = Enrolments(Set(
    Enrolment("HMRC-PODS-ORG", Seq(
      EnrolmentIdentifier("PSAID", "A0000000")
    ), "Activated", None)
  ))

  private val emptyCredentials = new~(new~(None, emptyEnrolments), None)
  private val testCredentials = new~(new~(Some("Ext-137d03b9-d807-4283-a254-fb6c30aceef1"), enrolments), Some(Name(Some("firstName"), Some("lastName"))))



  before {
    reset(mockAuthConnector)
    reset(mockJSONPayloadSchemaValidator)
    reset(mockEventReportService)
    when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn
      Future.successful(new~(new~(Some(externalId), enrolments), Some(Name(Some("FirstName"), Some("lastName")))))
    when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any())).thenReturn(Failure(EventReportValidationFailureException("Test")))
  }

  "getOverview" must {
    "return OK with the overview payload returned from service" in {
      when(mockEventReportService.getOverview(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(endDate))(any(), any()))
        .thenReturn(Future.successful(erOverviewResponseJson))

      val result = controller.getOverview(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "startDate" -> startDate, "endDate" -> endDate))

      whenReady(result) { _ =>
        status(result) mustBe OK
        contentAsJson(result) mustBe erOverviewResponseJson
      }
    }

    "throw a Bad Request Exception when endDate parameter is missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "startDate" -> "2022-04-06"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("endDate missing")
      }
    }

    "throw a Bad Request Exception when startDate parameter is missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "endDate" -> "2022-04-06"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("startDate missing")
      }
    }

    "throw a Bad Request Exception when pstr parameter is missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "startDate" -> "2022-04-06", "endDate" -> "2022-04-06"))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("pstr missing")
      }
    }

    "throw a Bad Request Exception when all required parameters are missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getOverview()(fakeRequest)
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("pstr missing")
        response.message must include("startDate missing")
        response.message must include("endDate missing")
      }
    }

    "throw a Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn Future.successful(emptyCredentials)

      recoverToExceptionIf[UnauthorizedException] {
        controller.getOverview()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "fromDate" -> "2021-04-06", "toDate" -> "2022-04-05"))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "submitEventDeclarationReport" must {

    "return BadRequest when the scheme is already registered  by the user within the TTL" in {
      when(mockEventReportService.submitEventDeclarationReport(any(), any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.successful(BadRequest))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any()))
        .thenReturn(Success(()))

      val result = controller.submitEventDeclarationReport(fakeRequest.withJsonBody(submitEventDeclarationReportSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr, "version" -> reportVersion))
      status(result) mustBe BAD_REQUEST
    }

    "return NoContent when valid response" in {
      when(mockEventReportService.submitEventDeclarationReport(any(), any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.successful(NoContent))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any()))
        .thenReturn(Success(()))

      val result = controller.submitEventDeclarationReport(fakeRequest.withJsonBody(submitEventDeclarationReportSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr, "version" -> reportVersion))

      status(result) mustBe NO_CONTENT
    }

    "throw a Bad Request Exception when the body is missing" in {
      recoverToExceptionIf[BadRequestException] {
        controller.submitEventDeclarationReport(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "version" -> reportVersion))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Request does not contain required Json body")
      }
    }

    "throw a Bad Request Exception when the pstr is missing from the header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.submitEventDeclarationReport(fakeRequest.withJsonBody(submitEventDeclarationReportSuccessResponse))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("""pstr missing""")
      }
    }
  }

  "submitEvent20ADeclarationReport" must {
    "return NoContent when valid response" in {
      when(mockEventReportService.submitEvent20ADeclarationReport(any(), any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.successful(NoContent))
      when(mockJSONPayloadSchemaValidator.validatePayload(any(), any(), any()))
        .thenReturn(Success(()))

      val result = controller.submitEvent20ADeclarationReport(fakeRequest.withJsonBody(submitEvent20ADeclarationReportSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr, "version" -> reportVersion))

      status(result) mustBe NO_CONTENT
    }
    "throw a Bad Request Exception when the pstr is missing from the header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.submitEvent20ADeclarationReport(fakeRequest.withJsonBody(submitEvent20ADeclarationReportSuccessResponse))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("""pstr missing""")
      }
    }
    "throw a Bad Request Exception when the body is missing" in {
      recoverToExceptionIf[BadRequestException] {
        controller.submitEvent20ADeclarationReport(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "version" -> reportVersion))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST

        response.message must include("Request does not contain required Json body")
      }
    }
  }
  "getVersions" must {
    "return OK with the Seq of Version" in {
      when(mockEventReportService.getVersions(
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(startDate))(any(), any()))
        .thenReturn(Future.successful(erVersions))

      val result = controller.getVersions(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "startDate" -> startDate))

      status(result) mustBe OK
      contentAsJson(result) mustBe erVersions
    }

    "throw a Bad Request Exception when startDate parameter is missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getVersions()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include("startDate missing")
      }
    }
    "throw a Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn Future.successful(emptyCredentials)

      recoverToExceptionIf[UnauthorizedException] {
        controller.getVersions()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "startDate" -> "2021-04-06"))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "isEventDataChanged" must {
    "return true when event data is changed" in {
      when(mockEventReportService.isNewReportDifferentToPrevious(any(), any(), any(), any(), any(), any())(any(), any()))
                .thenReturn(Future.successful(true))

      val result = controller.isEventDataChanged(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "eventType" -> "1", "year" -> "2024", "version" -> "2"))

      status(result) mustBe OK
      contentAsString(result) mustBe true.toString
    }

    "throw a Bad Request Exception when startDate parameter is missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.isEventDataChanged(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include("version missing  eventType missing  year missing")
      }
    }
    "throw a Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn Future.successful(emptyCredentials)

      recoverToExceptionIf[UnauthorizedException] {
        controller.isEventDataChanged()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "startDate" -> "2021-04-06"))
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
        ArgumentMatchers.eq("001"),
        ArgumentMatchers.eq(startDate),
        any(),
        any(),
        any()
      )(any(), any()))
        .thenReturn(Future.successful(dummyJsValue))

      val result = controller.getEventSummary(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr,
        "reportStartDate" -> startDate,
        "reportVersionNumber" -> reportVersion
      ))

      status(result) mustBe OK
      contentAsJson(result) mustBe dummyJsValue
    }

    "throw a Bad Request Exception when all parameters missing in header" in {
      recoverToExceptionIf[BadRequestException] {
        controller.getEventSummary(fakeRequest)
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include("pstr missing")
        response.message must include("reportVersionNumber missing")
        response.message must include("reportStartDate missing")
      }
    }

    "throw a Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn Future.successful(emptyCredentials)

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
        ArgumentMatchers.eq(externalId),
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(Event1: EventType),
        ArgumentMatchers.eq(2020),
        ArgumentMatchers.eq(1),
        any()
      )(any(), any()))
        .thenReturn(Future.successful(Some(json)))

      val result = controller.getUserAnswers(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "year" -> "2020", "version" -> reportVersion, "eventType" -> eventType, externalId -> externalId))

      status(result) mustBe OK
    }

    "return not found exception when invalid event type" in {

      recoverToExceptionIf[NotFoundException] {
        controller.getUserAnswers(fakeRequest.withHeaders(
          newHeaders = "pstr" -> pstr, "year" -> "2020", "version" -> reportVersion, "eventType" -> "test", externalId -> externalId))
      } map { response =>
        verify(mockEventReportService, never).getUserAnswers(any(), any(), any(), any(), any(), any())(any(), any())
        response.responseCode mustBe NOT_FOUND
        response.message must include("Bad Request: eventType (test) not found")
      }
    }

    "return none when not found" in {

      when(mockEventReportService.getUserAnswers(
        ArgumentMatchers.eq(externalId),
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(Event1),
        ArgumentMatchers.eq(2020),
        ArgumentMatchers.eq(1),
        any()
      )(any(), any()))
        .thenReturn(Future.successful(None))

      val result = controller.getUserAnswers(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "year" -> "2020", "version" -> reportVersion, "eventType" -> eventType, externalId -> externalId))

      status(result) mustBe NOT_FOUND

    }

    "return OK when eventType missing" in {

      when(mockEventReportService.getUserAnswers(
        ArgumentMatchers.eq(externalId),
        any()
      )(any()))
        .thenReturn(Future.successful(Some(json)))

      val result = controller.getUserAnswers(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "year" -> "2020", "version" -> reportVersion))

      status(result) mustBe OK
    }

    "throw a 401 Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn Future.successful(emptyCredentials)

      recoverToExceptionIf[UnauthorizedException] {
        controller.getUserAnswers(fakeRequest.withHeaders(
          newHeaders = "pstr" -> pstr, "year" -> "2020", "version" -> reportVersion, "eventType" -> eventType, externalId -> externalId))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "removeUserAnswers" must {
    "return 200 OK when no reportVersion" in {

      when(mockEventReportService.removeUserAnswers(
        ArgumentMatchers.eq(externalId)
      )(any()))
        .thenReturn(Future.successful(()))

      val result = controller.removeUserAnswers(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, externalId -> externalId))

      status(result) mustBe OK
    }

    "throw a UnauthorizedException when not authorized" in {
      when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn Future.successful(emptyCredentials)
      recoverToExceptionIf[UnauthorizedException] {
        controller.removeUserAnswers(fakeRequest)
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
      }
    }
  }

  "saveUserAnswersToCache" must {
    "return 200 OK when valid response" in {

      when(mockEventReportService.saveUserAnswers(
        ArgumentMatchers.eq(externalId),
        ArgumentMatchers.eq(pstr),
        ArgumentMatchers.eq(Event1),
        ArgumentMatchers.eq(2020),
        ArgumentMatchers.eq(1),
        any(),
        any(),
        any()
      )(any()))
        .thenReturn(Future.successful())

      val result = controller.saveUserAnswers(fakeRequest.withJsonBody(saveUserAnswersToCacheSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr, "year" -> "2020", "version" -> reportVersion, "eventType" -> eventType, externalId -> externalId))

      status(result) mustBe OK
    }

    "return not found exception when invalid event type" in {

      recoverToExceptionIf[NotFoundException] {
        controller.saveUserAnswers(fakeRequest.withJsonBody(saveUserAnswersToCacheSuccessResponse).withHeaders(
          newHeaders = "pstr" -> pstr, "year" -> "2020", "version" -> reportVersion, "eventType" -> "test"))
      } map { response =>
        verify(mockEventReportService, never).saveUserAnswers(any(), any(), any(), any(), any(), any(), any(), any())(any())
        response.responseCode mustBe NOT_FOUND
        response.message must include("Bad Request: eventType (test) not found")
      }
    }

    "return OK when eventType missing" in {
      when(mockEventReportService.saveUserAnswers(
        ArgumentMatchers.eq(externalId),
        any(),
        any()
      )(any()))
        .thenReturn(Future.successful(()))

      val result = controller.saveUserAnswers(fakeRequest.withJsonBody(saveUserAnswersToCacheSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr, "year" -> "2020", "version" -> reportVersion, externalId -> externalId))

      status(result) mustBe OK
    }

    "throw a 401 Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn Future.successful(emptyCredentials)

      recoverToExceptionIf[UnauthorizedException] {
        controller.saveUserAnswers()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr, "eventType" -> eventType)
          .withJsonBody(saveUserAnswersToCacheSuccessResponse))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
    "return error if locked" in {
      when(mockEventReportService.saveUserAnswers(any(), any(), any(), any(), any(), any(), any(), any())(any()))
        .thenReturn(Future.failed(UserLockedException(Some("A0000000"))))
      recoverToExceptionIf[UserLockedException] {
        controller.saveUserAnswers()(fakeRequest.withHeaders(
              newHeaders = "pstr" -> pstr, "year" -> "2020", "version" -> reportVersion, "eventType" -> eventType, externalId -> externalId
          )
          .withJsonBody(saveUserAnswersToCacheSuccessResponse))
      } map { response =>
        response.psaOrPspId mustBe Some("A0000000")
      }
    }
  }

  "changeVersion" must {
    "return 204 OK when valid response" in {
      when(mockEventReportService.changeVersion(
        ArgumentMatchers.eq(externalId),
        any(),
        ArgumentMatchers.eq(1),
        ArgumentMatchers.eq(2)
      )(any()))
        .thenReturn(Future.successful(Some(mock[UpdateResult])))
      val result = controller.changeVersion(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "year" -> "2020", "version" -> reportVersion, "eventType" -> eventType, externalId -> externalId, "newVersion" -> "2"))

      status(result) mustBe NO_CONTENT
    }

    "return 400 OK when not found" in {
      when(mockEventReportService.changeVersion(
        ArgumentMatchers.eq(externalId),
        any(),
        ArgumentMatchers.eq(1),
        ArgumentMatchers.eq(2)
      )(any()))
        .thenReturn(Future.successful(None))
      val result = controller.changeVersion(fakeRequest.withHeaders(
        newHeaders = "pstr" -> pstr, "year" -> "2020", "version" -> reportVersion, "eventType" -> eventType, externalId -> externalId, "newVersion" -> "2"))

      status(result) mustBe NOT_FOUND
    }
  }

  "compileEvent" must {
    "return 204 No Content when valid response" in {
      when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn
        Future.successful(testCredentials)
      when(mockEventReportService.compileEventReport(any(), any(), any(), any(), any(), any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.successful(NoContent))

      val result = controller.compileEvent(fakeRequest.withJsonBody(compileEventSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr, "eventType" -> "1", "year" -> "2020", "currentVersion" -> "1", "reportVersion" -> "1", externalId -> externalId, "version" -> reportVersion))

      status(result) mustBe NO_CONTENT
    }

    "throw a 401 Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn
        Future.successful(emptyCredentials)
      recoverToExceptionIf[UnauthorizedException] {
        controller.compileEvent()(fakeRequest.withJsonBody(compileEventSuccessResponse)
          .withHeaders(newHeaders = "pstr" -> pstr, "eventType" -> "1", externalId -> externalId, "version" -> reportVersion))
      } map { response =>
        response.responseCode mustBe UNAUTHORIZED
        response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
      }
    }
  }

  "deleteMember" must {
    "return 204 No Content when valid response" in {
      when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn
        Future.successful(testCredentials)
      when(mockEventReportService.deleteMember(any(), any(), any(), any(), any(), any(), any(), any())(any(), any(), any()))
        .thenReturn(Future.successful(NoContent))

      val result = controller.deleteMember()(fakeRequest.withJsonBody(compileEventSuccessResponse).withHeaders(
        newHeaders = "pstr" -> pstr,
        "eventType" -> "1",
        "year" -> "2020",
        "reportVersion" -> "1",
        externalId -> externalId,
        "version" -> reportVersion,
        "memberIdToDelete" -> "0",
        "currentVersion" -> "1")
      )

      status(result) mustBe NO_CONTENT
    }

    "throw a 401 Unauthorised Exception if auth fails" in {
      when(mockAuthConnector.authorise[Option[String] ~ Enrolments ~ Option[Name]](any(), any())(any(), any())) thenReturn
        Future.successful(emptyCredentials)
      recoverToExceptionIf[UnauthorizedException] {
        controller.deleteMember()(fakeRequest.withJsonBody(compileEventSuccessResponse)
          .withHeaders(newHeaders =
            "pstr" -> pstr,
            "eventType" -> "1",
            externalId -> externalId,
            "version" -> reportVersion,
            "memberIdToDelete" -> "0"))
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
  private val externalId = "externalId"

  private val eventType = "1"
  private val reportVersion = "1"

  private val startDate = "2022-04-06"
  private val endDate = "2023-04-05"

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

  private val submitEventDeclarationReportSuccessResponse: JsObject = Json.obj("processingDate" -> "2023-06-14",
    "formBundleNumber" -> "12345678933")

  private val submitEvent20ADeclarationReportSuccessResponse: JsObject = Json.obj("processingDate" -> "2023-06-14",
    "formBundleNumber" -> "12345670811")

  private val erVersions = Json.arr(Json.obj(
    "versionDetails" -> Json.obj(
      "version"-> 1,
      "status" -> "compiled",
      "submittedDate" -> startDate,
      "submitterName" ->  "ABC Limited"
    )))


  private val saveUserAnswersToCacheSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678955")

  private val compileEventSuccessResponse: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678977")

  private val json = Json.obj("test" -> "test")
}

