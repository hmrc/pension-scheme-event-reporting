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
import models.enumeration.EventType.Event6
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}

import scala.util.Random

trait GeneratorAPI1830 extends Matchers with OptionValues with ResponseGenerators {

  private val typesOfProtection = Seq(
    "enhancedLifetimeAllowance",
    "enhancedProtection",
    "fixedProtection",
    "fixedProtection2014",
    "fixedProtection2016",
    "individualProtection2014",
    "individualProtection2016"
  )

  def generateUserAnswersAndPOSTBodyNew(eventType: EventType): Gen[Tuple2[JsObject, JsObject]] = {
    eventType match {
      case Event6 => generateUserAnswersAndPOSTBodyEvent6(Event6)
      case _ => generateUserAnswersAndPOSTBodyEvent22And23(eventType)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent6(eventType: EventType): Gen[Tuple2[JsObject, JsObject]] = {
    for {
      firstName <- Gen.oneOf(Seq("Alice", "Bob", "Charlie"))
      lastName <- Gen.oneOf(Seq("Xavier", "Yilmaz", "Zimmer"))
      nino <- Gen.oneOf(Seq("AB123456C", "CD123456E"))
      typeOfProtection <- Gen.oneOf(typesOfProtection)
      inputProtectionType <- Gen.chooseNum(10000000, 99999999)
      amountCrystallised <- Gen.chooseNum(1, 1000)
      taxYear <- taxYearGenerator
    } yield {
      val ua = Json.obj(
        s"event${eventType.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> firstName,
                "lastName" -> lastName,
                "nino" -> nino),
              "typeOfProtection" -> typeOfProtection,
              "inputProtectionType" -> inputProtectionType,
              "AmountCrystallisedAndDate" -> Json.obj(
                "amountCrystallised" -> amountCrystallised,
                "crystallisedDate" -> s"$taxYear-04-25"
              )
            )
          )
        ),
        "taxYear" -> taxYear
      )
      val endTaxYear = (taxYear.toInt + 1).toString
      val expected = Json.obj("memberEventsDetails" -> Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> "87219363YN",
          "eventType" -> s"Event${eventType.toString}",
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"$endTaxYear-04-05"
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
              "amountCrystalised" -> amountCrystallised,
              "typeOfProtection" -> typeOfProtection,
              "eventDate" -> s"$taxYear-04-25",
              "freeText" -> inputProtectionType
            )
          )
        )
      )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent22And23(eventType: EventType): Gen[Tuple2[JsObject, JsObject]] = {
    for {
      firstName <- Gen.alphaStr
      lastName <- Gen.alphaStr
      nino <- Gen.oneOf(Seq("AB123456C", "CD123456E"))
      monetaryAmount <- arbitrary[BigDecimal]
      taxYearEndDate <- Gen.oneOf(2020, 2021, 2022)
      taxYear <- taxYearGenerator
    } yield {
      val ua = Json.obj(
        s"event${eventType.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> firstName,
                "lastName" -> lastName,
                "nino" -> nino),
              "chooseTaxYear" -> taxYearEndDate.toString,
              "totalPensionAmounts" -> monetaryAmount
            ))
        ),
        "taxYear" -> taxYear
      )
      val endTaxYear = (taxYear.toInt + 1).toString
      val expected = Json.obj("memberEventsDetails" -> Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> "87219363YN",
          "eventType" -> s"Event${eventType.toString}",
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"$endTaxYear-04-05"
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
