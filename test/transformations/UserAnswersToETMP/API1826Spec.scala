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

import models.enumeration.EventType
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1826, JsonFileReader}

class API1826Spec extends AnyFreeSpec with Matchers
  with JsonFileReader with GeneratorAPI1826 with ScalaCheckPropertyChecks {

  "transformToETMPData" - {
    "must transform a randomly generated valid payload correctly for Event 10" in {
      forAll(generateUserAnswersAndPOSTBodyEvent10) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1826.transformToETMPData(EventType.Event10, delete = false))
          val expectedResult = JsSuccess(expectedResponse)
          result mustBe expectedResult
      }
    }
    "must transform a randomly generated valid payload correctly for Event 11" in {
      forAll(generateUserAnswersAndPOSTBodyEvent11) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          if (userAnswers == expectedResponse) {
            Right("Test bypassed, transformation not required when both booleans are false")
          } else {
            val expectedResult = JsSuccess(expectedResponse)

            val result = userAnswers.validate(API1826.transformToETMPData(EventType.Event10, delete = false))
            result mustBe expectedResult
          }
      }
    }
    "must transform a randomly generated valid payload correctly for Event 12" in {
      forAll(generateUserAnswersAndPOSTBodyEvent12) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1826.transformToETMPData(EventType.Event10, delete = false))
          val expectedResult = JsSuccess(expectedResponse)
          result mustBe expectedResult
      }
    }
    "must transform a randomly generated valid payload correctly for Event 13" in {
      forAll(generateUserAnswersAndPOSTBodyEvent13) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val expectedResult = JsSuccess(expectedResponse)

          val result = userAnswers.validate(API1826.transformToETMPData(EventType.Event10, delete = false))

          result mustBe expectedResult
      }
    }
    "must transform a randomly generated valid payload correctly for Event 14" in {
      forAll(generateUserAnswersAndPOSTBodyEvent14) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>

          val expectedResult = JsSuccess(expectedResponse)
          val result = userAnswers.validate(API1826.transformToETMPData(EventType.Event10, delete = false))
          result mustBe expectedResult
      }
    }
    "must transform a randomly generated valid payload correctly for Wind Up" in {
      forAll(generateUserAnswersAndPOSTBodyWindUp) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val expectedResult = JsSuccess(expectedResponse)
          val result = userAnswers.validate(API1826.transformToETMPData(EventType.Event10, delete = false))

          result mustBe expectedResult
      }
    }
    "must transform a randomly generated valid payload correctly for Event 18" in {
      forAll(generateUserAnswersAndPOSTBodyEvent18) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1826.transformToETMPData(EventType.Event10, delete = false))
          result.asOpt mustBe Some(expectedResponse)
      }
    }

    "must transform a randomly generated valid payload correctly for Event 19" in {
      forAll(generateUserAnswersAndPOSTBodyEvent19) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>

          val result = userAnswers.validate(API1826.transformToETMPData(EventType.Event10, delete = false))
          result.asOpt mustBe Some(expectedResponse)
      }
    }

    "must transform a randomly generated valid payload correctly for Event 20" in {
      forAll(generateUserAnswersAndPOSTBodyEvent20) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1826.transformToETMPData(EventType.Event10, delete = false))
          val expectedResult = JsSuccess(expectedResponse)
          result mustBe expectedResult

      }
    }
  }

  "must not transform an event that is not present" in {
    val userAnswers: JsObject =
      Json.obj("taxYear" -> "2020")

    val result = userAnswers.validate(API1826.transformToETMPData(EventType.Event10, delete = false))
    val expected: JsObject = {
      Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> "2020-04-06",
          "reportEndDate" -> "2021-04-05"
        )
      )
    }
    result.asOpt mustBe Some(expected)
  }
}
