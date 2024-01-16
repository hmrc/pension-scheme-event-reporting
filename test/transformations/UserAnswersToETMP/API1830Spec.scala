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

package transformations.UserAnswersToETMP

import models.enumeration.EventType._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1830, JsonFileReader}

class API1830Spec extends AnyFreeSpec with Matchers
  with JsonFileReader with GeneratorAPI1830 with ScalaCheckPropertyChecks {

  "transformToETMPData" - {

    "must transform a randomly generated valid payload correctly in event 2" in {
      forAll(generateUserAnswersAndPOSTBodyByEvent(Event2)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event2, "87219363YN"))
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event2") \ Symbol("members"))
          result mustBe expectedResult
      }
    }

    "must transform a randomly generated valid payload correctly in event 3" in {
      forAll(generateUserAnswersAndPOSTBodyByEvent(Event3)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event3, "87219363YN"))
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event3") \ Symbol("members"))
          result mustBe expectedResult
      }
    }

    "must transform a randomly generated valid payload correctly in event 4" in {
      forAll(generateUserAnswersAndPOSTBodyByEvent(Event4)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event4, "87219363YN"))
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event4") \ Symbol("members"))
          result mustBe expectedResult
      }
    }

    "must transform a randomly generated valid payload correctly in event 5" in {
      forAll(generateUserAnswersAndPOSTBodyByEvent(Event5)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event5, "87219363YN"))
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event5") \ Symbol("members"))
          result mustBe expectedResult
      }
    }

    "must transform a randomly generated valid payload correctly in event 6" in {
      forAll(generateUserAnswersAndPOSTBodyByEvent(Event6)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event6, "87219363YN"))
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event6") \ Symbol("members"))
          result mustBe expectedResult
      }
    }

    "must transform a randomly generated valid payload correctly in event 7" in {
      forAll(generateUserAnswersAndPOSTBodyByEvent(Event7)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event7, "87219363YN"))
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event7") \ Symbol("members"))
          result mustBe expectedResult
      }
    }

    "must transform a randomly generated valid payload correctly in event 8" in {
      forAll(generateUserAnswersAndPOSTBodyByEvent(Event8)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event8, "87219363YN"))
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event8") \ Symbol("members"))
          result mustBe expectedResult
      }
    }

    "must transform a randomly generated valid payload correctly in event 8A" in {
      forAll(generateUserAnswersAndPOSTBodyByEvent(Event8A)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event8A, "87219363YN"))
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event8A") \ Symbol("members"))
          result mustBe expectedResult
      }
    }

    "must transform a randomly generated valid payload correctly in event 22" in {
      forAll(generateUserAnswersAndPOSTBodyByEvent(Event22)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event22, "87219363YN"))
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event22") \ Symbol("members"))
          result mustBe expectedResult
      }
    }

    "must transform a randomly generated valid payload correctly in event 23" in {
      forAll(generateUserAnswersAndPOSTBodyByEvent(Event23)) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event23, "87219363YN"))
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event23") \ Symbol("members"))
          result mustBe expectedResult
      }
    }

    "must correctly handle no member submit" in {
      forAll(generateEmptyUserAnswersAndPOSTBodyEvent2) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1830.transformToETMPData(Event2, "87219363YN"))
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event2") \ Symbol("members"))
          result mustBe expectedResult
      }
    }
  }
}

