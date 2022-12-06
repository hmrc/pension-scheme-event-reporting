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
import utils.{GeneratorAPI1826, JsonFileReader}

class API1826Spec extends AnyFreeSpec with Matchers
  with JsonFileReader with GeneratorAPI1826 with ScalaCheckPropertyChecks {

  "transformToETMPData" - {
    "must transform a randomly generated valid payload correctly for Wind Up" in {
      forAll(generateUserAnswersAndPOSTBodyWindUp) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1826.transformToETMPData)
          val expectedResult = JsSuccess(Some(expectedResponse), __ \ "schemeWindUpDate")
          result mustBe expectedResult
      }
    }
    "must transform a randomly generated valid payload correctly for Event 18" in {
      forAll(generateUserAnswersAndPOSTBodyEvent18) {
        case (userAnswers: JsObject, expectedResponse: Option[JsObject]) =>
          val result = userAnswers.validate(API1826.transformToETMPData)
          result.asOpt.flatten mustBe expectedResponse
      }
    }

    "must transform all events when present" in {
      val userAnswers: JsObject = {
        Json.obj(
          "event18Confirmation" -> true,
          "schemeWindUpDate" -> "1991-11-22"
        )
      }
      val expected: JsObject = {
        Json.obj(
          "eventReportDetails" -> Json.obj(
            "reportStartDate" -> "2020-09-01",
            "reportEndDate" -> "2020-09-01"
          ),
          "eventDetails" -> Json.obj(
            "eventWindUp" -> Json.obj(
              "dateOfWindUp" -> "1991-11-22"
            ),
            "event18" -> Json.obj(
              "chargeablePmt"->"Yes"
            )
          )
        )
      }
      val result = userAnswers.validate(API1826.transformToETMPData)
      result.asOpt.flatten mustBe Some(expected.as[JsObject])
    }

    "must not transform an event that is not present" in {
      val userAnswers: JsObject = {
        Json.obj()
      }
      val result = userAnswers.validate(API1826.transformToETMPData)
      val expected: JsObject = {
        Json.obj(
          "eventReportDetails" -> Json.obj(
            "reportStartDate" -> "2020-09-01",
            "reportEndDate" -> "2020-09-01"
          )
        )
      }
      result.asOpt.flatten mustBe Some(expected)
    }
  }
}

