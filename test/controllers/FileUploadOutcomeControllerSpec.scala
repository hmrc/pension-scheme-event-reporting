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

import actions.AuthAction
import org.apache.pekko.util.ByteString
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{reset, when}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.json.{JsObject, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.{Application, inject}
import repositories.FileUploadResponseCacheRepository
import uk.gov.hmrc.http._
import utils.AuthUtils
import utils.AuthUtils.FakeAuthAction

import scala.concurrent.Future
import scala.util.Random

class FileUploadOutcomeControllerSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfterEach {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  private val mockFileUploadResponseCache = mock[FileUploadResponseCacheRepository]
  private val fakeRequest = FakeRequest()
  private val fakePostRequest = FakeRequest("POST", "/")


  private def modules: Seq[GuiceableModule] = {
    Seq(
      inject.bind[FileUploadResponseCacheRepository].toInstance(mockFileUploadResponseCache),
      inject.bind[AuthAction].to[FakeAuthAction]
    )
  }

  private val application: Application = new GuiceApplicationBuilder()
    .configure(conf = "auditing.enabled" -> false, "metrics.enabled" -> false, "metrics.jvm" -> false).
    overrides(modules: _*).build()

  private val controller: FileUploadOutcomeController = application.injector.instanceOf[FileUploadOutcomeController]
  private val srn = AuthUtils.srn
  override protected def beforeEach(): Unit = {
    super.beforeEach()
    reset(mockFileUploadResponseCache)
  }

  private val returnedFileUploadResponse: JsObject =
    Json.obj(
      "reference" -> "123"
    )

  "get" must {
    "return OK with the data" in {
      when(mockFileUploadResponseCache.get(ArgumentMatchers.eq("123"))(any())) thenReturn Future.successful(Some(Json.obj("reference" -> "123")))
      val result = controller.get(fakeRequest.withHeaders(newHeaders = "reference" -> "123"))
      status(result) mustEqual OK
      contentAsJson(result) mustEqual returnedFileUploadResponse
    }

    "return NOT FOUND when the data doesn't exist" in {
      when(mockFileUploadResponseCache.get(ArgumentMatchers.eq("123"))(any())) thenReturn Future.successful(None)
      val result = controller.get(fakeRequest.withHeaders(newHeaders = "reference" -> "123"))
      status(result) mustEqual NOT_FOUND
    }

    "return internal server error when the repository call fails" in {
      when(mockFileUploadResponseCache.get(ArgumentMatchers.eq("123"))(any())) thenReturn Future.failed(new Exception())
      val result = controller.get(fakeRequest.withHeaders(newHeaders = "reference" -> "123"))
      status(result) mustBe INTERNAL_SERVER_ERROR
    }
  }

  "save" must {
    "return OK when data is saved successfully" in {
      when(mockFileUploadResponseCache.upsert(any(), any())(any())) thenReturn Future.successful((): Unit)
      val result = controller.save(fakePostRequest.withJsonBody(Json.obj("reference" -> "123")))
      status(result) mustEqual OK
    }

    "throw an exception when the request body cannot be parsed" in {
      when(mockFileUploadResponseCache.upsert(any(), any())(any())) thenReturn Future.successful(None)
      val result = controller.save(fakePostRequest.withRawBody(ByteString(Random.alphanumeric.dropWhile(_.isDigit).take(20).mkString)))
      status(result) mustBe BAD_REQUEST
      contentAsString(result) mustBe "No JSON body"
    }
  }

  "getSrn" must {
    "return OK with the data" in {
      when(mockFileUploadResponseCache.get(ArgumentMatchers.eq("123"))(any())) thenReturn Future.successful(Some(Json.obj("reference" -> "123")))
      val result = controller.getSrn(srn)(fakeRequest.withHeaders(newHeaders = "reference" -> "123"))
      status(result) mustEqual OK
      contentAsJson(result) mustEqual returnedFileUploadResponse
    }

    "return NOT FOUND when the data doesn't exist" in {
      when(mockFileUploadResponseCache.get(ArgumentMatchers.eq("123"))(any())) thenReturn Future.successful(None)
      val result = controller.getSrn(srn)(fakeRequest.withHeaders(newHeaders = "reference" -> "123"))
      status(result) mustEqual NOT_FOUND
    }

    "return Internal server error when the repository call fails" in {
      when(mockFileUploadResponseCache.get(ArgumentMatchers.eq("123"))(any())) thenReturn Future.failed(new Exception())
      val result = controller.getSrn(srn)(fakeRequest.withHeaders(newHeaders = "reference" -> "123"))
      status(result) mustBe INTERNAL_SERVER_ERROR
    }
  }

  "saveSrn" must {
    "return OK when data is saved successfully" in {
      when(mockFileUploadResponseCache.upsert(any(), any())(any())) thenReturn Future.successful((): Unit)
      val result = controller.saveSrn(srn)(fakePostRequest.withJsonBody(Json.obj("reference" -> "123")))
      status(result) mustEqual OK
    }

    "throw an exception when the request body cannot be parsed" in {
      when(mockFileUploadResponseCache.upsert(any(), any())(any())) thenReturn Future.successful(None)
      val result = controller.saveSrn(srn)(fakePostRequest.withRawBody(ByteString(Random.alphanumeric.dropWhile(_.isDigit).take(20).mkString)))
      status(result) mustBe BAD_REQUEST
      contentAsString(result) mustBe "No JSON body"
    }
  }
}



