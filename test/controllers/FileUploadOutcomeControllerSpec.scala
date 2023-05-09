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

///*
// * Copyright 2023 HM Revenue & Customs
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package controllers
//
//import com.github.tomakehurst.wiremock.client.WireMock.{aResponse, equalTo, post, urlEqualTo}
//import org.scalatest.BeforeAndAfter
//import org.scalatest.matchers.must.Matchers
//import org.scalatest.wordspec.AsyncWordSpec
//import org.scalatestplus.mockito.MockitoSugar
//import play.api.libs.json.{JsObject, Json}
//import play.api.test.Helpers._
//import play.api.test.Helpers.baseApplicationBuilder.injector
//import uk.gov.hmrc.http._
//import utils.WireMockHelper
//
//class FileUploadOutcomeControllerSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfter {
//
//  implicit val hc: HeaderCarrier = HeaderCarrier()
//  private lazy val controller: FileUploadOutcomeController = injector.instanceOf[FileUploadOutcomeController]
//
//  private val returnedFileUploadResponse: JsObject =
//    Json.obj(
//      "reference" -> "123"
//    )
//
//  "save" must {
//    "save the data in the collection" in {
//      val result = controller.get
//    }
//  }
//}
//
//
//
