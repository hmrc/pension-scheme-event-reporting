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

import models.enumeration.EventType.{Event22, Event23}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1830, JsonFileReader}

class API1830Spec extends AnyFreeSpec with Matchers
  with JsonFileReader with GeneratorAPI1830 with ScalaCheckPropertyChecks {

  "transformToETMPData" - {
    "must transform a randomly generated valid payload correctly in event 23" in {
      forAll(generateUserAnswersAndPOSTBody(Event23)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event23,"87219363YN"))
          val expectedResult = JsSuccess(Some(expectedResponse), __ \ Symbol("event23") \ Symbol("members"))
          result mustBe expectedResult
      }
    }

    "must transform a randomly generated valid payload correctly in event 22" in {
      forAll(generateUserAnswersAndPOSTBody(Event22)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event22,"87219363YN"))
          val expectedResult = JsSuccess(Some(expectedResponse), __ \ Symbol("event22") \ Symbol("members"))
          result mustBe expectedResult
      }
    }
  }
}

