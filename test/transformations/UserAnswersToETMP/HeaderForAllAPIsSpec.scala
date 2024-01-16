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

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1826, JsonFileReader}

class HeaderForAllAPIsSpec extends AnyFreeSpec with Matchers
  with JsonFileReader with GeneratorAPI1826 with ScalaCheckPropertyChecks {
  "transformToETMPData" - {
    "must transform a the header details correctly" in {
      val userAnswers = Json.obj("taxYear" -> "2022")

      val exp = Json.obj("eventReportDetails" -> Json.obj(
        "reportStartDate" -> "2022-04-06",
        "reportEndDate" -> "2023-04-05"
      ))

      val expectedResult = JsSuccess(exp)

      val result = userAnswers.validate(HeaderForAllAPIs.transformToETMPData())
      result mustBe expectedResult
    }
  }
}

