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

package utils

import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}

trait GeneratorAPI1826 extends Matchers with OptionValues with ResponseGenerators {
  def generateUserAnswersAndPOSTBody: Gen[(JsObject, JsObject)] = {
    for {
      schemeWindUpDate <- dateGenerator
    } yield {
      val fullUA = Json.obj(
        "schemeWindUpDate" -> schemeWindUpDate
      )
      val fullExpectedResult = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> "2020-09-01",
          "reportEndDate" -> "2020-09-01"
        ),
        "eventDetails" -> Json.obj(
          "eventWindUp" -> Json.obj(
            "dateOfWindUp" -> schemeWindUpDate
          )
        )
      )
      Tuple2(fullUA, fullExpectedResult)
    }
  }
}
