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

import models.enumeration.EventType
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}

trait GeneratorAPI1830 extends Matchers with OptionValues with ResponseGenerators {

  def generateUserAnswersAndPOSTBody(eventType: EventType): Gen[Tuple2[JsObject, JsObject]] = {
    for {
      firstName <- Gen.alphaStr
      lastName <- Gen.alphaStr
      nino <- Gen.oneOf(Seq("AB123456C", "CD123456E"))
      monetaryAmount <- arbitrary[BigDecimal]
      taxYearEndDate <- Gen.oneOf(2020, 2021, 2022)
    } yield {
      val ua = Json.obj(s"event${eventType.toString}" -> Json.obj("members" ->
        Json.arr(
          Json.obj(
            "membersDetails" -> Json.obj(
              "firstName" -> firstName,
              "lastName" -> lastName,
              "nino" -> nino),
            "chooseTaxYear" -> taxYearEndDate.toString,
            "totalPensionAmounts" -> monetaryAmount
          ))
      ))
      val expected = Json.obj("memberEventsDetails" -> Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> "87219363YN",
          "eventType" -> s"Event${eventType.toString}",
          "reportStartDate" -> "2020-09-01",
          "reportEndDate" -> "2020-09-01"
        ),
        "eventDetails" -> Json.arr(
          Json.obj(
            "eventType" -> s"Event${eventType.toString}",
            "individualDetails" -> Json.obj(
              "firstName" -> firstName,
              "lastName" -> lastName,
              "nino" -> nino
            ),
            "paymentDetails" -> Json.obj(
              "monetaryAmount" -> monetaryAmount,
              "taxYearEndingDate" -> (taxYearEndDate + 1).toString
            )
          )
        )
      )
      )
      Tuple2(ua, expected)
    }
  }
}
