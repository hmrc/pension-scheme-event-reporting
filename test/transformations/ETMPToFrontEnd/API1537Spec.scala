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

package transformations.ETMPToFrontEnd

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1537, JsonFileReader}

class API1537Spec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader with ScalaCheckPropertyChecks with GeneratorAPI1537 {

  "Reads" - {
    "transform a valid payload from API 1537 correctly" in {
      forAll(generateGetVersionJson) {
        case (payload: JsArray, expected: JsArray) =>
          val result = payload.validate(API1537.reads)
          result mustBe JsSuccess(expected)
      }
    }

    "transform a valid payload from API 1537 correctly missing reportSubmitterDetails" in {
      forAll(generateGetVersionMissingSubmitterJson) {
        case (payload: JsArray, expected: JsArray) => {
          val result = payload.validate(API1537.reads)
          result mustBe JsSuccess(expected)
        }
      }
    }
  }
}
