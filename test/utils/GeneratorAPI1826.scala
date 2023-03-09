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

package utils

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}

trait GeneratorAPI1826 extends Matchers with OptionValues with ResponseGenerators {
  def generateUserAnswersAndPOSTBodyWindUp: Gen[(JsObject, JsObject)] = {
    for {
      schemeWindUpDate <- dateGenerator
      taxYear <- taxYearGenerator
    } yield {
      val endTaxYear = (taxYear.toInt + 1).toString
      val fullUA = Json.obj(
        "schemeWindUpDate" -> schemeWindUpDate,
        "taxYear" -> taxYear
      )
      val fullExpectedResult = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"$endTaxYear-04-05"
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

  def generateUserAnswersAndPOSTBodyEvent18: Gen[(JsObject, JsObject)] = {
    for {
      event18Confirmation <- arbitrary[Option[Boolean]]
      taxYear <- taxYearGenerator
    } yield {
      val endTaxYear = (taxYear.toInt + 1).toString
      val fullUA = event18Confirmation match {
        case Some(value) => Json.obj(
          "event18Confirmation" -> value,
          "taxYear" -> taxYear
        )
        case None => Json.obj(
          "taxYear" -> taxYear
        )
      }

      val fullExpectedResult =
        event18Confirmation match {
          case Some(true) => Json.obj(
            "eventReportDetails" -> Json.obj(
              "reportStartDate" -> s"$taxYear-04-06",
              "reportEndDate" -> s"$endTaxYear-04-05"
            ),
            "eventDetails" -> Json.obj("event18" -> Json.obj(
              "chargeablePmt" -> "Yes"
            )
            )
          )
          case _ => Json.obj(
            "eventReportDetails" -> Json.obj(
              "reportStartDate" -> s"$taxYear-04-06",
              "reportEndDate" -> s"$endTaxYear-04-05"
            ),
          )
        }

      Tuple2(fullUA, fullExpectedResult)
    }
  }
}
