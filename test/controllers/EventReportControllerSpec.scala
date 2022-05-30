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
import org.mockito.ArgumentMatchers.any
import org.mockito.MockitoSugar
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

    "return OK when validation errors response" in {
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

    "throw BadRequestException when PSTR is not present in the header" in {

      val controller = application.injector.instanceOf[EventReportController]

      recoverToExceptionIf[BadRequestException] {
        controller.compileEventReportSummary()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr))
      } map { response =>
        response.responseCode mustBe BAD_REQUEST
        response.message must include(s"Bad Request without pstr (Some($pstr)) or request body (None)")
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
}

object EventReportControllerSpec {
  val pstr = "pstr"
  val compileEventReportSummaryResponseJson: JsObject = Json.obj("processingDate" -> LocalDate.now(),
    "formBundleNumber" -> "12345678912")
}
