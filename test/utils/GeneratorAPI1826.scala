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

//noinspection ScalaStyle
trait GeneratorAPI1826 extends Matchers with OptionValues with ResponseGenerators {

  def generateUserAnswersAndPOSTBodyEvent10: Gen[(JsObject, JsObject)] = {
    for {
      version <- Gen.oneOf(1,2)
      becomeOrCeaseScheme <- Gen.oneOf(Seq("itBecameAnInvestmentRegulatedPensionScheme", "itHasCeasedToBeAnInvestmentRegulatedPensionScheme"))
      taxYear <- taxYearGenerator
      contractsOrPolicies <- arbitrary[Boolean]
    } yield {

      def event10DetailsUA(becomeOrCeaseScheme: String): JsObject = {
        becomeOrCeaseScheme match {
          case "itBecameAnInvestmentRegulatedPensionScheme" =>
            Json.obj(
              "recordVersion" -> version,
              "becomeOrCeaseScheme" -> becomeOrCeaseScheme,
              "schemeChangeDate" -> Json.obj(
                "schemeChangeDate" -> s"$taxYear-04-06"
              ),
              "contractsOrPolicies" -> contractsOrPolicies
            )
          case _ =>
            Json.obj(
              "recordVersion" -> version,
              "becomeOrCeaseScheme" -> becomeOrCeaseScheme,
              "schemeChangeDate" -> Json.obj(
                "schemeChangeDate" -> s"$taxYear-04-06"
              )
            )
        }
      }

      def event10DetailsExpected(becomeOrCeaseScheme: String): JsObject = {
        becomeOrCeaseScheme match {
          case "itBecameAnInvestmentRegulatedPensionScheme" =>
            Json.obj(
              "startDateDetails" -> Json.obj(
                "startDateOfInvReg" -> s"$taxYear-04-06",
                "contractsOrPolicies" -> (if (contractsOrPolicies) "Yes" else "No")
              )
            )
          case _ =>
            Json.obj(
              "ceaseDateDetails" -> Json.obj(
                "ceaseDateOfInvReg" -> s"$taxYear-04-06"
              )
            )
        }
      }

      val ua = Json.obj(
        "event10" -> event10DetailsUA(becomeOrCeaseScheme),
        "taxYear" -> taxYear
      )
      val expected = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"${taxYear.toInt + 1}-04-05"
        ),
        "eventDetails" -> Json.obj(
          "event10" -> Json.arr(
            Json.obj(
              "recordVersion" -> ("00" + version.toString),
              "invRegScheme" -> event10DetailsExpected(becomeOrCeaseScheme)
            )
          )
        )
      )
      Tuple2(ua, expected)
    }
  }


  def generateUserAnswersAndPOSTBodyEvent11: Gen[(JsObject, JsObject)] = {
    for {
      version <- Gen.oneOf(1,2)
      taxYear <- taxYearGenerator
      whichTest <- arbitrary[Boolean]
    } yield {
      val (hasSchemeChangedRulesUnAuthPayments, hasSchemeChangedRulesInvestmentsInAssets) =  whichTest match {
        case true => (true, false)
        case false => (false, true)
      }

      def event11DetailsUA(unAuthPayments: Boolean, investmentsInAssets: Boolean): JsObject = {
          (unAuthPayments, investmentsInAssets) match {
            case (true, true) =>
              Json.obj(
                "recordVersion" -> version,
                "hasSchemeChangedRulesUnAuthPayments" -> unAuthPayments,
                "unAuthPaymentsRuleChangeDate" -> Json.obj(
                  "date" -> s"$taxYear-08-06"
                ),
                "hasSchemeChangedRulesInvestmentsInAssets" -> investmentsInAssets,
                "investmentsInAssetsRuleChangeDate" -> Json.obj(
                  "date" -> s"$taxYear-08-30"
                )
              )
            case (false, true) =>
              Json.obj(
                "recordVersion" -> version,
                "hasSchemeChangedRulesUnAuthPayments" -> unAuthPayments,
                "hasSchemeChangedRulesInvestmentsInAssets" -> investmentsInAssets,
                "investmentsInAssetsRuleChangeDate" -> Json.obj(
                  "date" -> s"$taxYear-08-30"
                )
              )
            case (true, false) =>
              Json.obj(
                "recordVersion" -> version,
                "hasSchemeChangedRulesUnAuthPayments" -> unAuthPayments,
                "unAuthPaymentsRuleChangeDate" -> Json.obj(
                  "date" -> s"$taxYear-08-06"
                ),
                "hasSchemeChangedRulesInvestmentsInAssets" -> investmentsInAssets
              )
            case _ =>
              Json.obj()
          }

        }

        def event11DetailsExpected(unAuthPayments: Boolean, investmentsInAssets: Boolean): JsObject = {
          (unAuthPayments, investmentsInAssets) match {
            case (true, true) =>
              Json.obj(
                "recordVersion" -> ("00" + version.toString),
                "unauthorisedPmtsDate" -> s"$taxYear-08-06",
                "contractsOrPoliciesDate" -> s"$taxYear-08-30"
              )
            case (false, true) =>
              Json.obj(
                "recordVersion" -> ("00" + version.toString),
                "contractsOrPoliciesDate" -> s"$taxYear-08-30"
              )
            case (true, false) =>
              Json.obj(
                "recordVersion" -> ("00" + version.toString),
                "unauthorisedPmtsDate" -> s"$taxYear-08-06"
              )
            case _ =>
              Json.obj()
          }
        }

        val ua = Json.obj(
          "event11" -> event11DetailsUA(hasSchemeChangedRulesUnAuthPayments, hasSchemeChangedRulesInvestmentsInAssets),
          "taxYear" -> taxYear
        )
        val expected = Json.obj(
          "eventReportDetails" -> Json.obj(
            "reportStartDate" -> s"$taxYear-04-06",
            "reportEndDate" -> s"${taxYear.toInt + 1}-04-05"
          ),
          "eventDetails" -> Json.obj(
            "event11" -> event11DetailsExpected(hasSchemeChangedRulesUnAuthPayments, hasSchemeChangedRulesInvestmentsInAssets)
          )
        )
        Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent12: Gen[(JsObject, JsObject)] = {
    for {
      version <- Gen.oneOf(1, 2)
      taxYear <- taxYearGenerator
    } yield {
      val ua = Json.obj(
        "event12" -> Json.obj(
          "recordVersion" -> version,
          "hasSchemeChangedRules" -> true,
          "dateOfChange" -> Json.obj(
            "dateOfChange" -> s"${taxYear}-04-06"
          )
        ),
        "taxYear" -> taxYear
      )
      val expected = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"${taxYear.toInt + 1}-04-05"
        ),
        "eventDetails" -> Json.obj(
          "event12" -> Json.obj(
            "recordVersion" -> ("00" + version.toString),
            "twoOrMoreSchemesDate" -> s"${taxYear}-04-06"
          )
        )
      )
      Tuple2(ua, expected)
    }
  }


  private val schemeStructureETMPEvent13: Map[String, String] = {
    Map("single" -> "A single trust under which all of the assets are held for the benefit of all members of the scheme",
      "group" -> "A group life/death in service scheme",
      "corporate" -> "A body corporate",
      "other" -> "Other")
  }

  def generateUserAnswersAndPOSTBodyEvent13: Gen[(JsObject, JsObject)] = {
    for {
      version <- Gen.oneOf(1, 2)
      taxYear <- taxYearGenerator
      schemeStructureUA <- Gen.oneOf(schemeStructureETMPEvent13.keys.toSeq)
      schemeStructureDescription <- Gen.alphaStr.suchThat(_.nonEmpty)
      isSchemeStructureDescPresent <- Gen.oneOf(Seq(true, false))
    } yield {
      def event13DetailsUA: JsObject = {
        (schemeStructureUA, isSchemeStructureDescPresent) match {
          case ("other", true) =>
            Json.obj(
              "recordVersion" -> version,
              "recordVersion" -> "001",
              "schemeStructure" -> schemeStructureUA,
              "changeDate" -> s"$taxYear-04-06",
              "schemeStructureDescription" -> schemeStructureDescription
            )
          case ("other", false) =>
            Json.obj(
              "recordVersion" -> version,
              "recordVersion" -> "001",
              "schemeStructure" -> schemeStructureUA,
              "changeDate" -> s"$taxYear-04-06"
            )
          case _ => Json.obj(
            "recordVersion" -> version,
            "recordVersion" -> "001",
            "schemeStructure" -> schemeStructureUA,
            "changeDate" -> s"$taxYear-04-06"
          )
        }
      }

      def event13DetailsExpected: JsObject = {
        (schemeStructureUA, isSchemeStructureDescPresent) match {
          case ("other", true) => Json.obj(
            "recordVersion" -> ("00" + version.toString),
            "schemeStructure" -> schemeStructureETMPEvent13(schemeStructureUA),
            "dateOfChange" -> s"$taxYear-04-06",
            "schemeStructureOther" -> schemeStructureDescription
          )
          case ("other", false) => Json.obj(
            "recordVersion" -> ("00" + version.toString),
            "schemeStructure" -> schemeStructureETMPEvent13(schemeStructureUA),
            "dateOfChange" -> s"$taxYear-04-06"
          )
          case _ => Json.obj(
            "recordVersion" -> ("00" + version.toString),
            "schemeStructure" -> schemeStructureETMPEvent13(schemeStructureUA),
            "dateOfChange" -> s"$taxYear-04-06"
          )
        }
      }

      val ua = Json.obj(
        "event13" -> event13DetailsUA,
        "taxYear" -> taxYear
      )
      val expected = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"${taxYear.toInt + 1}-04-05"
        ),
        "eventDetails" -> Json.obj(
          "event13" -> Json.arr(
            event13DetailsExpected
          )
        )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent14: Gen[(JsObject, JsObject)] = {
    for {
      version <- Gen.oneOf(1, 2)
      taxYear <- taxYearGenerator
      schemeMembers <- Gen.oneOf(Seq("0", "1", "2 to 11", "12 to 50", "51 to 10,000", "More than 10,000"))
    } yield {
      val ua = Json.obj(
        "event14" -> Json.obj(
          "recordVersion" -> version,
          "schemeMembers" -> schemeMembers
        ),
        "taxYear" -> taxYear
      )
      val expected = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"${taxYear.toInt + 1}-04-05"
        ),
        "eventDetails" -> Json.obj(
          "event14" -> Json.obj(
            "recordVersion" -> ("00" + version.toString),
            "schemeMembers" -> schemeMembers
          )
        )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyWindUp: Gen[(JsObject, JsObject)] = {
    for {
      version <- Gen.oneOf(1, 2)
      schemeWindUpDate <- dateGenerator
      taxYear <- taxYearGenerator
    } yield {
      val endTaxYear = (taxYear.toInt + 1).toString
      val fullUA = Json.obj(
        "eventWindUp" -> Json.obj(
          "recordVersion" -> version,
          "schemeWindUpDate" -> schemeWindUpDate
        ),
        "taxYear" -> taxYear
      )
      val fullExpectedResult = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"$endTaxYear-04-05"
        ),
        "eventDetails" -> Json.obj(
          "eventWindUp" -> Json.obj(
            "recordVersion" -> ("00" + version.toString),
            "dateOfWindUp" -> schemeWindUpDate
          )
        )
      )
      Tuple2(fullUA, fullExpectedResult)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent18: Gen[(JsObject, JsObject)] = {
    for {
      version <- Gen.oneOf(1, 2)
      taxYear <- taxYearGenerator
    } yield {
      val endTaxYear = (taxYear.toInt + 1).toString
      val fullUA = Json.obj(
        "event18" -> Json.obj(
          "recordVersion" -> version,
          "event18Confirmation" -> true,
        ),
        "taxYear" -> taxYear
      )

      val fullExpectedResult = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"$endTaxYear-04-05"
        ),
        "eventDetails" -> Json.obj(
          "event18" -> Json.obj(
            "recordVersion" -> ("00" + version.toString),
            "chargeablePmt" -> "Yes"
          )
        )
      )
      Tuple2(fullUA, fullExpectedResult)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent19: Gen[(JsObject, JsObject)] = {
    for {
      version <- Gen.oneOf(1, 2)
      taxYear <- taxYearGenerator
      countryOrTerritory <- Gen.listOfN(2, nonEmptyString).map(_.mkString)
    } yield {
      val endTaxYear = (taxYear.toInt + 1).toString
      val fullUA = Json.obj(
        "event19" -> Json.obj(
          "recordVersion" -> version,
          "CountryOrTerritory" -> countryOrTerritory,
          "dateChangeMade" -> s"$taxYear-12-21"
        ),
        "taxYear" -> taxYear
      )

      val fullExpectedResult = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"$endTaxYear-04-05"
        ),
        "eventDetails" -> Json.obj(
          "event19" -> Json.arr(
            Json.obj(
              "recordVersion" -> ("00" + version.toString),
              "countryCode" -> countryOrTerritory,
              "dateOfChange" -> s"$taxYear-12-21"
            )
          )
        )
      )
      Tuple2(fullUA, fullExpectedResult)
    }
  }
  
  def generateUserAnswersAndPOSTBodyEvent20: Gen[(JsObject, JsObject)] = {
    for {
      version <- Gen.oneOf(1, 2)
      whatChange <- Gen.oneOf(Seq("becameOccupationalScheme", "ceasedOccupationalScheme"))
      taxYear <- taxYearGenerator
    } yield {

      def event20DetailsUA(whatChange: String): JsObject = {
        whatChange match {
          case "becameOccupationalScheme" =>
            Json.obj(
              "recordVersion" -> version,
              "whatChange" -> whatChange,
              "becameDate" -> Json.obj(
                "date" -> s"$taxYear-04-06"
              )
            )
          case _ =>
            Json.obj(
              "recordVersion" -> version,
              "whatChange" -> whatChange,
              "ceasedDate" -> Json.obj(
                "date" -> s"$taxYear-04-06"
              )
            )
        }
      }

      def event20DetailsExpected(whatChange: String): JsObject = {
        whatChange match {
          case "becameOccupationalScheme" =>
            Json.obj(
                "startDateOfOccScheme" -> s"$taxYear-04-06",
              )
          case _ =>
            Json.obj(
                "stopDateOfOccScheme" -> s"$taxYear-04-06"
              )
        }
      }

      val ua = Json.obj(
        "event20" -> event20DetailsUA(whatChange),
        "taxYear" -> taxYear
      )
      val expected = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"${taxYear.toInt + 1}-04-05"
        ),
        "eventDetails" -> Json.obj(
          "event20" -> Json.arr(
            Json.obj(
              "recordVersion" -> ("00" + version.toString),
              "occSchemeDetails" -> event20DetailsExpected(whatChange)
            )
          )
        )
      )
      Tuple2(ua, expected)
    }
  }
}