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

package transformations.UserAnswersToETMP

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1827, JsonFileReader}

class API1827Spec extends AnyFreeSpec with Matchers
  with JsonFileReader with GeneratorAPI1827 with ScalaCheckPropertyChecks {

  "transformToETMPData" - {
    "must transform a randomly generated valid payload correctly" in {
      forAll(generateUserAnswersAndPOSTBody) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1827.transformToETMPData)
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event1") \ Symbol("membersOrEmployers"))
          result mustBe expectedResult
      }
    }
  }
}

