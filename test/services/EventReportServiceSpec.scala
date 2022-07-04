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

package services

import connectors.EventReportConnector
import org.mockito.ArgumentMatchers.any
import org.mockito.MockitoSugar
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.json.Json
import repositories.EventReportCacheRepository
import uk.gov.hmrc.http._
import utils.JSONPayloadSchemaValidator

class EventReportServiceSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfter {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  private val mockEventReportConnector = mock[EventReportConnector]
  private val mockJSONPayloadSchemaValidator = mock[JSONPayloadSchemaValidator]
  private val mockEventReportCacheRepository = mock[EventReportCacheRepository]
  val modules: Seq[GuiceableModule] =
    Seq(
      bind[EventReportConnector].toInstance(mockEventReportConnector),
      bind[JSONPayloadSchemaValidator].toInstance(mockJSONPayloadSchemaValidator),
      bind[EventReportCacheRepository].toInstance(mockEventReportCacheRepository)
    )

  val application: Application = new GuiceApplicationBuilder()
    .configure(conf = "auditing.enabled" -> false, "metrics.enabled" -> false, "metrics.jvm" -> false).
    overrides(modules: _*).build()

  val eventReportService = new EventReportService(mockEventReportConnector, mockEventReportCacheRepository, mockJSONPayloadSchemaValidator)

  before {
    reset(mockEventReportConnector)
    when(mockJSONPayloadSchemaValidator.validateJsonPayload(any(), any())) thenReturn Right(true)
  }

  //  "compileEventReport" must {
  //    "return 204 No Content when valid response for EventReportSummary" in {
  //      val service = application.injector.instanceOf[EventReportService]
  //
  //      when(mockEventReportConnector.compileEventReportSummary(any(), any())(any(), any()))
  //        .thenReturn(Future.successful(HttpResponse(OK, compileEventReportSuccessResponse.toString)))
  //
  //      when(mockEventReportCacheRepository.getByKeys(any())(any()))
  //        .thenReturn(Future.successful(Some(compileEventReportSuccessResponse)))
  //
  //      when(mockEventReportCacheRepository.getByKeys(any())(any()))
  //        .thenReturn(Future.successful(Some(compileEventReportSuccessResponse)))
  //
  //      val result = service.compileEventReport("pstr", compileEventReportSuccessResponse)
  //      status(result) mustBe NO_CONTENT
  //    }

  //    "return 204 No Content when valid response for EventOneReport" in {
  //      val service = eventReportService
  //
  //      when(mockEventReportConnector.compileEventOneReport(any(), any())(any(), any()))
  //        .thenReturn(Future.successful(HttpResponse(OK, compileEventReportSuccessResponse.toString)))
  //
  //      when(mockEventReportCacheRepository.getByKeys(any())(any()))
  //        .thenReturn(Future.successful(None))
  //
  //      val result = service.compileEventReport("pstr", compileEventReportSuccessResponse)
  //      status(result) mustBe NO_CONTENT
  //    }


//  "return 400 when validation errors response" in {
//    val controller = application.injector.instanceOf[EventReportController]
//
//    when(mockEventReportConnector.compileEventReportSummary(any(), any())(any(), any()))
//      .thenReturn(Future.successful(HttpResponse(OK, compileEventReportSummaryResponseJson.toString)))
//
//    val listErrors: List[ErrorReport] = List(
//      ErrorReport("instance1", "errors1"),
//      ErrorReport("instance2", "errors2")
//    )
//
//    when(mockJSONPayloadSchemaValidator.validateJsonPayload(any(), any())) thenReturn Left(listErrors)
//
//    recoverToExceptionIf[EventReportValidationFailureException] {
//      controller.compileEventReportSummary(fakeRequest.withJsonBody(compileEventReportSummaryResponseJson).withHeaders(
//        newHeaders = "pstr" -> pstr))
//    } map {
//      failure =>
//        failure.exMessage mustBe "Schema validation errors:-\n(instance1: errors1),\n(instance2: errors2)"
//    }
//  }
//
//  "throw Upstream5XXResponse on Internal Server Error" in {
//    val controller = application.injector.instanceOf[EventReportController]
//
//    when(mockEventReportConnector.compileEventReportSummary(any(), any())(any(), any()))
//      .thenReturn(Future.failed(UpstreamErrorResponse(message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR)))
//    recoverToExceptionIf[UpstreamErrorResponse] {
//      controller.compileEventReportSummary(fakeRequest.withJsonBody(compileEventReportSummaryResponseJson).
//        withHeaders(newHeaders = "pstr" -> pstr))
//    } map {
//      _.statusCode mustBe INTERNAL_SERVER_ERROR
//    }
//  }
//
//  "throw BadRequestException when request body not provided" in {
//
//    val controller = application.injector.instanceOf[EventReportController]
//
//    recoverToExceptionIf[BadRequestException] {
//      controller.compileEventReportSummary()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr))
//    } map { response =>
//      response.responseCode mustBe BAD_REQUEST
//      response.message must include(s"Bad Request without pstr (Some($pstr)) or request body (None)")
//    }
//  }
//
//  "throw BadRequestException when PSTR missing in header" in {
//
//    val controller = application.injector.instanceOf[EventReportController]
//
//    recoverToExceptionIf[BadRequestException] {
//      controller.compileEventReportSummary(fakeRequest.withJsonBody(compileEventReportSummaryResponseJson))
//    } map { response =>
//      response.responseCode mustBe BAD_REQUEST
//      response.message must include(s"Bad Request without pstr (None) or request body (Some($compileEventReportSummaryResponseJson))")
//    }
//  }
//
//  "throw Unauthorized exception if auth fails" in {
//    when(authConnector.authorise[Option[String]](any(), any())(any(), any())) thenReturn Future.successful(None)
//    val controller = application.injector.instanceOf[EventReportController]
//
//    recoverToExceptionIf[UnauthorizedException] {
//      controller.compileEventReportSummary()(fakeRequest.withHeaders(newHeaders = "pstr" -> pstr))
//    } map { response =>
//      response.responseCode mustBe UNAUTHORIZED
//      response.message must include("Not Authorised - Unable to retrieve credentials - externalId")
//    }
//  }
//
//}
//
//"compileEventReportSummary" must {
//"return OK when valid response" in {
//val controller = application.injector.instanceOf[EventReportController]
//
//when (mockEventReportConnector.compileEventReportSummary (any (), any () ) (any (), any () ) )
//.thenReturn (Future.successful (HttpResponse (OK, compileEventReportSummaryResponseJson.toString) ) )
//
//val result = controller.compileEventReportSummary (fakeRequest.withJsonBody (compileEventReportSummaryResponseJson).withHeaders (
//newHeaders = "pstr" -> pstr) )
//status (result) mustBe OK
//}
//
//"return 400 when validation errors response" in {
//val controller = application.injector.instanceOf[EventReportController]
//
//when (mockEventReportConnector.compileEventReportSummary (any (), any () ) (any (), any () ) )
//.thenReturn (Future.successful (HttpResponse (OK, compileEventReportSummaryResponseJson.toString) ) )
//
//val listErrors: List[ErrorReport] = List (
//ErrorReport ("instance1", "errors1"),
//ErrorReport ("instance2", "errors2")
//)
//
//when (mockJSONPayloadSchemaValidator.validateJsonPayload (any (), any () ) ) thenReturn Left (listErrors)
//
//recoverToExceptionIf[EventReportValidationFailureException] {
//controller.compileEventReportSummary (fakeRequest.withJsonBody (compileEventReportSummaryResponseJson).withHeaders (
//newHeaders = "pstr" -> pstr) )
//} map {
//failure =>
//failure.exMessage mustBe "Schema validation errors:-\n(instance1: errors1),\n(instance2: errors2)"
//}
//}
//
//"throw Upstream5XXResponse on Internal Server Error" in {
//val controller = application.injector.instanceOf[EventReportController]
//
//when (mockEventReportConnector.compileEventReportSummary (any (), any () ) (any (), any () ) )
//.thenReturn (Future.failed (UpstreamErrorResponse (message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR) ) )
//recoverToExceptionIf[UpstreamErrorResponse] {
//controller.compileEventReportSummary (fakeRequest.withJsonBody (compileEventReportSummaryResponseJson).
//withHeaders (newHeaders = "pstr" -> pstr) )
//} map {
//_.statusCode mustBe INTERNAL_SERVER_ERROR
//}
//}
//
//"throw BadRequestException when request body not provided" in {
//
//val controller = application.injector.instanceOf[EventReportController]
//
//recoverToExceptionIf[BadRequestException] {
//controller.compileEventReportSummary () (fakeRequest.withHeaders (newHeaders = "pstr" -> pstr) )
//} map {
//response =>
//response.responseCode mustBe BAD_REQUEST
//response.message must include (s"Bad Request without pstr (Some($pstr)) or request body (None)")
//}
//}
//
//"throw BadRequestException when PSTR missing in header" in {
//
//val controller = application.injector.instanceOf[EventReportController]
//
//recoverToExceptionIf[BadRequestException] {
//controller.compileEventReportSummary (fakeRequest.withJsonBody (compileEventReportSummaryResponseJson) )
//} map {
//response =>
//response.responseCode mustBe BAD_REQUEST
//response.message must include (s"Bad Request without pstr (None) or request body (Some($compileEventReportSummaryResponseJson))")
//}
//}
//
//"throw Unauthorized exception if auth fails" in {
//when (authConnector.authorise[Option[String]] (any (), any () ) (any (), any () ) ) thenReturn Future.successful (None)
//val controller = application.injector.instanceOf[EventReportController]
//
//recoverToExceptionIf[UnauthorizedException] {
//controller.compileEventReportSummary () (fakeRequest.withHeaders (newHeaders = "pstr" -> pstr) )
//} map {
//response =>
//response.responseCode mustBe UNAUTHORIZED
//response.message must include ("Not Authorised - Unable to retrieve credentials - externalId")
//}
//}
//
//}
//
//"compileEventReportOne" must {
//"return OK when valid response" in {
//val controller = application.injector.instanceOf[EventReportController]
//
//when (mockEventReportConnector.compileEventOneReport (any (), any () ) (any (), any () ) )
//.thenReturn (Future.successful (HttpResponse (OK, compileEventOneReportSuccessResponse.toString) ) )
//
//val result = controller.compileEventOneReport (fakeRequest.withJsonBody (compileEventOneReportSuccessResponse).withHeaders (
//newHeaders = "pstr" -> pstr) )
//
//status (result) mustBe OK
//}
//
//"return OK when validation errors response" in {
//val controller = application.injector.instanceOf[EventReportController]
//
//when (mockEventReportConnector.compileEventOneReport (any (), any () ) (any (), any () ) )
//.thenReturn (Future.successful (HttpResponse (OK, compileEventOneReportSuccessResponse.toString) ) )
//
//val listErrors: List[ErrorReport] = List (
//ErrorReport ("instance1", "errors1"),
//ErrorReport ("instance2", "errors2")
//)
//
//when (mockJSONPayloadSchemaValidator.validateJsonPayload (any (), any () ) ) thenReturn Left (listErrors)
//
//
//recoverToExceptionIf[EventReportValidationFailureException] {
//controller.compileEventOneReport (fakeRequest.withJsonBody (compileEventOneReportSuccessResponse).withHeaders (
//newHeaders = "pstr" -> pstr) )
//} map {
//failure =>
//failure.exMessage mustBe "Schema validation errors:-\n(instance1: errors1),\n(instance2: errors2)"
//}
//}
//
//"throw Upstream5XXResponse on Internal Server Error" in {
//val controller = application.injector.instanceOf[EventReportController]
//
//when (mockEventReportConnector.compileEventOneReport (any (), any () ) (any (), any () ) )
//.thenReturn (Future.failed (UpstreamErrorResponse (message = "Internal Server Error", INTERNAL_SERVER_ERROR, INTERNAL_SERVER_ERROR) ) )
//recoverToExceptionIf[UpstreamErrorResponse] {
//controller.compileEventOneReport (fakeRequest.withJsonBody (compileEventOneReportSuccessResponse).
//withHeaders (newHeaders = "pstr" -> pstr) )
//} map {
//_.statusCode mustBe INTERNAL_SERVER_ERROR
//}
//}
//
//"throw BadRequestException when request body not provided" in {
//
//val controller = application.injector.instanceOf[EventReportController]
//
//recoverToExceptionIf[BadRequestException] {
//controller.compileEventReportSummary () (fakeRequest.withHeaders (newHeaders = "pstr" -> pstr) )
//} map {
//response =>
//response.responseCode mustBe BAD_REQUEST
//response.message must include (s"Bad Request without pstr (Some($pstr)) or request body (None)")
//}
//}
//
//"throw BadRequestException when PSTR missing in header" in {
//
//val controller = application.injector.instanceOf[EventReportController]
//
//recoverToExceptionIf[BadRequestException] {
//controller.compileEventReportSummary (fakeRequest.withJsonBody (compileEventOneReportSuccessResponse) )
//} map {
//response =>
//response.responseCode mustBe BAD_REQUEST
//response.message must include (s"Bad Request without pstr (None) or request body (Some($compileEventOneReportSuccessResponse))")
//}
//}
//
//"throw Unauthorized exception if auth fails" in {
//when (authConnector.authorise[Option[String]] (any (), any () ) (any (), any () ) ) thenReturn Future.successful (None)
//val controller = application.injector.instanceOf[EventReportController]
//
//recoverToExceptionIf[UnauthorizedException] {
//controller.compileEventOneReport () (fakeRequest.withHeaders (newHeaders = "pstr" -> pstr) )
//} map {
//response =>
//response.responseCode mustBe UNAUTHORIZED
//response.message must include ("Not Authorised - Unable to retrieve credentials - externalId")
//}
//}
//}
//}
}

object EventReportServiceSpec {

  val compileEventReportSuccessResponse = Json.obj()
}


