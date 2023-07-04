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
import models.enumeration.EventType.{Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}


//noinspection ScalaStyle
trait GeneratorAPI1834 extends Matchers with OptionValues with ResponseGenerators {

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
      boolean <- Gen.oneOf(Seq(false, true))
      hasBecome <- Gen.oneOf(false, true)
      date <- dateGeneratorYMD
    } yield {

      val invRegSection = if (hasBecome) {
        Json.obj(
          "startDateDetails" -> Json.obj(
            "startDateOfInvReg" -> date,
            "contractsOrPolicies" -> toYesNo(boolean)
          )
        )
      } else {
        Json.obj(
          "ceaseDateDetails" -> Json.obj(
            "ceaseDateOfInvReg" -> date
          )
        )
      }

      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event10" -> Json.arr(
            Json.obj(
              "recordVersion" -> "001",
              "invRegScheme" -> invRegSection
            )
          )
        )
      )

      val whatChangedUA = if (hasBecome) {
        "itBecameAnInvestmentRegulatedPensionScheme"
      } else {
        "itHasCeasedToBeAnInvestmentRegulatedPensionScheme"
      }

      val conditionalJson = if (hasBecome) Json.obj("contractsOrPolicies" -> boolean) else Json.obj()

      val expected = Json.obj(
        "event10" -> (Json.obj(
          "becomeOrCeaseScheme" -> whatChangedUA,
          "schemeChangeDate" -> Json.obj(
            "schemeChangeDate" -> date
          )
        ) ++ conditionalJson)
      )

      Tuple2(payload, expected)
    }
  }

  private def generateForEvent11: Gen[(JsObject, JsObject)] = {
    for {
      date1 <- dateGeneratorYMD
      date2 <- dateGeneratorYMD
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event11" -> Json.obj(
            "recordVersion" -> "001",
            "unauthorisedPmtsDate" -> date1,
            "contractsOrPoliciesDate" -> date2
          )
        )
      )

      val expected = Json.obj(
        "event11" -> Json.obj(
          "hasSchemeChangedRulesUnAuthPayments" -> true,
          "unAuthPaymentsRuleChangeDate" -> Json.obj(
            "date" -> date1
          ),
          "hasSchemeChangedRulesInvestmentsInAssets" -> true,
          "investmentsInAssetsRuleChangeDate" -> Json.obj(
            "date" -> date2
          )
        )
      )
      Tuple2(payload, expected)
    }
  }

  private def generateForEvent12: Gen[(JsObject, JsObject)] = {
    for {
      date <- dateGeneratorYMD
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event12" -> Json.obj(
            "recordVersion" -> "001",
            "twoOrMoreSchemesDate" -> date
          )
        )
      )

      val expected = Json.obj(
        "event12" -> Json.obj(
          "hasSchemeChangedRules" -> true,
          "dateOfChange" -> Json.obj {
            "dateOfChange" -> date
          }
        )
      )
      Tuple2(payload, expected)
    }
  }

  private def generateForEvent13: Gen[(JsObject, JsObject)] = {
    for {
      date <- dateGeneratorYMD
      schemeStructure <- Gen.oneOf(
        "A single trust under which all of the assets are held for the benefit of all members of the scheme",
        "A group life/death in service scheme",
        "A body corporate",
        "Other")
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event13" -> Json.arr(
            Json.obj(
              "recordVersion" -> "001",
              "schemeStructure" -> schemeStructure,
              "dateOfChange" -> date
            )
          )
        )
      )

      def mapStructure(s: String): String = s match {
        case "A single trust under which all of the assets are held for the benefit of all members of the scheme" => "single"
        case "A group life/death in service scheme" => "group"
        case "A body corporate" => "corporate"
        case _ => "other"
      }

      val expected = Json.obj(
        "event13" -> Json.obj(
          "schemeStructure" -> mapStructure(schemeStructure),
          "changeDate" -> date
        )
      )
      Tuple2(payload, expected)
    }
  }


  private def generateForEvent14: Gen[(JsObject, JsObject)] = {
    for {
      members <- Gen.oneOf(
        "0",
        "1",
        "2 to 11",
        "12 to 50",
        "51 to 10,000",
        "More than 10,000"
      )
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event14" -> Json.obj(
            "recordVersion" -> "001",
            "schemeMembers" -> members
          )
        )
      )

      val expected = Json.obj(
        "event14" -> Json.obj(
          "schemeMembers" -> members
        )
      )
      Tuple2(payload, expected)
    }
  }


  private def generateForEvent18: Gen[(JsObject, JsObject)] = {
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


  private def generateForEvent19: Gen[(JsObject, JsObject)] = {
    for {
      date <- dateGeneratorYMD
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event19" -> Json.arr(
            Json.obj(
              "recordVersion" -> "001",
              "countryCode" -> "GB",
              "dateOfChange" -> date
            )
          )
        )
      )

      val expected = Json.obj(
        "event19" -> Json.obj(
          "CountryOrTerritory" -> "GB", // TODO: other countries
          "dateChangeMade" -> date
        )
      )
      Tuple2(payload, expected)
    }
  }


  private def generateForEvent20: Gen[(JsObject, JsObject)] = {
    for {
      date <- dateGeneratorYMD
      becameOrCeaseChange <- Gen.oneOf(Seq("becameOccupationalScheme", "ceasedOccupationalScheme"))
    } yield {
      def additionalPayloadChangeNode(payloadOrExpectedValue: String) = (payloadOrExpectedValue, becameOrCeaseChange) match {
        case ("payload", "becameOccupationalScheme") => Json.obj("startDateOfOccScheme" -> date)
        case ("payload", "ceasedOccupationalScheme") => Json.obj("stopDateOfOccScheme" -> date)
        case (_, "becameOccupationalScheme") => Json.obj(
          "becameDate" -> Json.obj("date" -> date),
          "whatChange" -> "becameOccupationalScheme")
        case _ => Json.obj(
          "ceasedDate" -> Json.obj("date" -> date),
          "whatChange" -> "ceasedOccupationalScheme")
      }

      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "event20" -> Json.arr(
            Json.obj(
              "recordVersion" -> "001",
              "occSchemeDetails" -> additionalPayloadChangeNode("payload")

            )
          )
        )
      )

      val expected = Json.obj("event20" -> additionalPayloadChangeNode("expected"))
      Tuple2(payload, expected)
    }
  }


  private def generateForEventWindup: Gen[(JsObject, JsObject)] = {
    for {
      date <- dateGeneratorYMD
    } yield {
      val payload: JsObject = Json.obj(
        "eventDetails" -> Json.obj(
          "eventWindUp" -> Json.obj(
            "recordVersion" -> "001",
            "dateOfWindUp" -> date
          )
        )
      )

      val expected = Json.obj(
        "eventWindUp" -> Json.obj(
          "schemeWindUpDate" -> date
        )
      )
      Tuple2(payload, expected)
    }
  }


}


object GeneratorAPI1834 extends ResponseGenerators {
  //  private def randomValues(): Gen[Map[String, String]] = {
  //    for {
  //      date <- dateGeneratorYMD
  ////      lastName <- Gen.oneOf(Seq("Xavier", "Yilmaz", "Zimmer"))
  ////      nino <- Gen.oneOf(Seq("AB123456C", "CD123456E"))
  ////      deceasedFirstName <- Gen.oneOf(Seq("Derek", "Emma", "Fred"))
  ////      deceasedLastName <- Gen.oneOf(Seq("Stevens", "Thomas", "Jones"))
  ////      deceasedNino <- Gen.oneOf(Seq("ZB123456C", "ZD123456E"))
  ////      reasonBenefitTakenEvent3 <- Gen.oneOf(Seq("Ill Health", "Protected Pension Age", "Other"))
  ////      pensionAmt <- Gen.chooseNum(1, 1000)
  ////      lumpSumAmount <- Gen.chooseNum(1, 1000)
  ////      typeOfProtectionEvent6 <- Gen.oneOf(Seq("Enhanced life time allowance",
  ////        "Enhanced protection", "Fixed protection", "Fixed protection 2014", "Fixed protection 2016",
  ////        "Individual protection 2014", "Individual protection 2016"))
  ////      inputProtectionType <- Gen.chooseNum(10000000, 99999999)
  ////      amountCrystallised <- Gen.chooseNum(1, 1000)
  ////      typeOfProtectionEvent8 <- Gen.oneOf(Seq("Primary Protection", "Enhanced protection"))
  ////      typeOfProtectionEvent8A <- Gen.oneOf(Seq("Primary Protection", "Enhanced"))
  ////      typeOfProtectionReference <- Gen.chooseNum(10000000, 99999999)
  ////      taxYearEndDate <- Gen.oneOf(2020, 2021, 2022)
  ////      reasonBenefitTakenEvent8A <- Gen.oneOf(
  ////        "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection",
  ////        "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance"
  ////      )
  //    } yield {
  //      Map(
  //        "firstName" -> firstName
  //      )
  //    }
  //  }
}
