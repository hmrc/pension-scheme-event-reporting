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

package transformations.UserAnswersToETMP

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1828, JsonFileReader}

// TODO: Consider relocation of code to different package.
class API1828Spec extends AnyFreeSpec with Matchers
  with JsonFileReader with GeneratorAPI1828 with ScalaCheckPropertyChecks {

  "transformToETMPData" - {
    "must transform a randomly generated valid payload correctly for Event Report Submit Declaration" in {
      forAll(generateUserAnswersAndPOSTBody) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1828.transformToETMPData)
          val expectedResult = JsSuccess(expectedResponse)
          // TODO: work out why it's adding pspDec2 path to result? Remove .gets to see.
          result.get mustBe expectedResult.get
      }
    }
  }
}

