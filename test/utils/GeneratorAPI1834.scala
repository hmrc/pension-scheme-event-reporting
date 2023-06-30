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
import models.enumeration.EventType.{Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event2, Event20, WindUp}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}


//noinspection ScalaStyle
trait GeneratorAPI1834 extends Matchers with OptionValues with ResponseGenerators {

  import GeneratorAPI1834._

  def generateUserAnswersAndPOSTBodyByEvent(eventType: EventType): Gen[(JsObject, JsObject)] = {
    eventType match {
      case Event10 => generateForEvent10
      case Event11 => generateForEvent11
      case Event12 => generateForEvent12
      case Event13 => generateForEvent13
      case Event14 => generateForEvent14
      case Event18 => generateForEvent18
      case Event19 => generateForEvent19
      case Event20 => generateForEvent20
      case _ => generateForEventWindup
    }
  }

  private def generateForEvent10: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event10" -> Json.arr(
            Json.obj(
              "recordVersion" -> "001",
              "invRegScheme" -> Json.obj(
                "startDateDetails" -> Json.obj(
                  "startDateOfInvReg" -> "2022-01-31",
                  "contractsOrPolicies" -> "Yes"
                )
              )
            )
          )
        )
      )

      val expected = Json.obj(
        "event10" -> Json.obj(
          "becomeOrCeaseScheme" -> "itBecameAnInvestmentRegulatedPensionScheme",
          "schemeChangeDate" -> Json.obj(
            "schemeChangeDate" -> "2024-01-01"
          ),
          "contractsOrPolicies" -> true
        )
      )

      Tuple2(payload, expected)

    }
  }

  private def generateForEvent11: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event11" -> Json.obj(
            "recordVersion" -> "001",
            "unauthorisedPmtsDate" -> "2022-01-31",
            "contractsOrPoliciesDate" -> "2022-01-31"
          )
        )
      )

      val expected = Json.obj(
        "event11" -> Json.obj(
          "hasSchemeChangedRulesUnAuthPayments" -> true,
          "unAuthPaymentsRuleChangeDate" -> Json.obj(
            "date" -> "2024-01-01"
          ),
          "hasSchemeChangedRulesInvestmentsInAssets" -> true,
          "investmentsInAssetsRuleChangeDate" -> Json.obj {
            "date" -> "2024-01-01"
          }
        )
      )
      Tuple2(payload, expected)
    }
  }

  private def generateForEvent12: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event12" -> Json.obj(
            "recordVersion" -> "001",
            "twoOrMoreSchemesDate" -> "2022-01-31"
          )
        )
      )

      val expected = Json.obj(
        "event12" -> Json.obj(
          "hasSchemeChangedRules" -> true,
          "dateOfChange" -> Json.obj {
            "dateOfChange" -> "2024-01-01"
          }
        )
      )
      Tuple2(payload, expected)
    }
  }

  private def generateForEvent13: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event13" -> Json.obj(
            "recordVersion" -> "001",
            "schemeStructure" -> "A single trust under which all of the assets are held for the benefit of all members of the scheme",
            "dateOfChange" -> "2024-01-01"
          )
        )
      )

      val expected = Json.obj(
        "event13" -> Json.obj(
          "schemeStructure" -> "single",
          "changeDate" -> "2024-01-01"
        )
      )
      Tuple2(payload, expected)
    }
  }


  private def generateForEvent14: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event14" -> Json.obj(
            "recordVersion" -> "001",
            "schemeMembers" -> "12 to 50"
          )
        )
      )

      val expected = Json.obj(
        "event14" -> Json.obj(
          "schemeMembers" -> "1"
        )
      )
      Tuple2(payload, expected)
    }
  }


  private def generateForEvent18: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event18" -> Json.obj(
            "recordVersion" -> "001",
            "chargeablePmt" -> "Yes"
          )
        )
      )

      val expected = Json.obj(
        "event18" -> Json.obj(
          "event18Confirmation" -> true
        )
      )
      Tuple2(payload, expected)
    }
  }


  private def generateForEvent19: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event19" -> Json.obj(
            "recordVersion" -> "001",
            "countryCode" -> "GB",
            "dateOfChange" -> "2024-01-01"
          )
        )
      )

      val expected = Json.obj(
        "event19" -> Json.obj(
          "CountryOrTerritory" -> "GB",
          "dateChangeMade" -> "2024-01-01"
        )
      )
      Tuple2(payload, expected)
    }
  }


  private def generateForEvent20: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event20" -> Json.obj(
            "recordVersion" -> "001",
            "occSchemeDetails" -> Json.obj(
              "startDateOfOccScheme" -> "2024-01-01"
            )
          )
        )
      )

      val expected = Json.obj(
        "event20" -> Json.obj(
          "whatChange" -> "becameOccupationalScheme",
          "becameDate" -> Json.obj {
            "date" -> "2024-01-01"
          }
        )
      )
      Tuple2(payload, expected)
    }
  }


  private def generateForEventWindup: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "eventWindUp" -> Json.obj(
            "recordVersion" -> "001",
            "dateOfWindUp" -> "2024-01-01"
          )
        )
      )

      val expected = Json.obj(
        "eventWindUp" -> Json.obj(
          "schemeWindUpDate" -> "2024-01-01"
        )
      )
      Tuple2(payload, expected)
    }
  }


}


object GeneratorAPI1834 {
  private def randomValues(): Gen[Map[String, String]] = {
    for {
      firstName <- Gen.oneOf(Seq("Alice", "Bob", "Charlie"))
      lastName <- Gen.oneOf(Seq("Xavier", "Yilmaz", "Zimmer"))
      nino <- Gen.oneOf(Seq("AB123456C", "CD123456E"))
      deceasedFirstName <- Gen.oneOf(Seq("Derek", "Emma", "Fred"))
      deceasedLastName <- Gen.oneOf(Seq("Stevens", "Thomas", "Jones"))
      deceasedNino <- Gen.oneOf(Seq("ZB123456C", "ZD123456E"))
      reasonBenefitTakenEvent3 <- Gen.oneOf(Seq("Ill Health", "Protected Pension Age", "Other"))
      pensionAmt <- Gen.chooseNum(1, 1000)
      lumpSumAmount <- Gen.chooseNum(1, 1000)
      typeOfProtectionEvent6 <- Gen.oneOf(Seq("Enhanced life time allowance",
        "Enhanced protection", "Fixed protection", "Fixed protection 2014", "Fixed protection 2016",
        "Individual protection 2014", "Individual protection 2016"))
      inputProtectionType <- Gen.chooseNum(10000000, 99999999)
      amountCrystallised <- Gen.chooseNum(1, 1000)
      typeOfProtectionEvent8 <- Gen.oneOf(Seq("Primary Protection", "Enhanced protection"))
      typeOfProtectionEvent8A <- Gen.oneOf(Seq("Primary Protection", "Enhanced"))
      typeOfProtectionReference <- Gen.chooseNum(10000000, 99999999)
      taxYearEndDate <- Gen.oneOf(2020, 2021, 2022)
      reasonBenefitTakenEvent8A <- Gen.oneOf(
        "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection",
        "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance"
      )
    } yield {
      Map(
        "firstName" -> firstName,
        "lastName" -> lastName,
        "nino" -> nino,
        "deceasedFirstName" -> deceasedFirstName,
        "deceasedLastName" -> deceasedLastName,
        "deceasedNino" -> deceasedNino,
        "reasonBenefitTakenEvent3" -> reasonBenefitTakenEvent3,
        "pensionAmt" -> pensionAmt.toString,
        "amountCrystallised" -> amountCrystallised.toString,
        "lumpSumAmount" -> lumpSumAmount.toString,
        "taxYearEndDate" -> taxYearEndDate.toString,
        "typeOfProtectionEvent6" -> typeOfProtectionEvent6,
        "inputProtectionType" -> inputProtectionType.toString,
        "typeOfProtectionEvent8" -> typeOfProtectionEvent8,
        "typeOfProtectionEvent8A" -> typeOfProtectionEvent8A,
        "typeOfProtectionReference" -> typeOfProtectionReference.toString,
        "reasonBenefitTakenEvent8A" -> reasonBenefitTakenEvent8A
      )
    }
  }
}
