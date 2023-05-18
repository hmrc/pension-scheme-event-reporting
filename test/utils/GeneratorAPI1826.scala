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
import models.enumeration.EventType._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}

//noinspection ScalaStyle
trait GeneratorAPI1826 extends Matchers with OptionValues with ResponseGenerators {
  def generateUserAnswersAndPOSTBodyByEvent(eventType: EventType): Gen[(JsObject, JsObject)] = {
    eventType match {
      case Event10 => generateUserAnswersAndPOSTBodyEvent10
      case _ => generateUserAnswersAndPOSTBodyWindUp
    }
  }

  /*
  "event10" : {
              "becomeOrCeaseScheme" : "itBecameAnInvestmentRegulatedPensionScheme",
              "schemeChangeDate" : {
                  "schemeChangeDate" : "2023-08-12"
              }
          }
      }

      "taxYear" : "2023",
          "event10" : {
              "becomeOrCeaseScheme" : "itHasCeasedToBeAnInvestmentRegulatedPensionScheme",
              "schemeChangeDate" : {
                  "schemeChangeDate" : "2023-08-12"
              },
              "contractsOrPolicies" : true
          }
      }
   */


  def contractsOrPoliciesNode(becomeOrCeaseSchemeValue: String, contractsOrPoliciesValue: Boolean): JsObject = {
    becomeOrCeaseSchemeValue match {
      case "itHasCeasedToBeAnInvestmentRegulatedPensionScheme" => Json.obj(
        "contractsOrPolicies" -> contractsOrPoliciesValue
      )
      case _ => Json.obj()
    }
  }

  def generateUserAnswersAndPOSTBodyEvent10: Gen[(JsObject, JsObject)] = {
    for {
      becomeOrCeaseScheme <- Gen.oneOf(Seq("itBecameAnInvestmentRegulatedPensionScheme", "itHasCeasedToBeAnInvestmentRegulatedPensionScheme"))
      taxYear <- taxYearGenerator
      contractsOrPolicies <- arbitrary[Boolean]
    } yield {
      val event10Details = Json.obj("becomeOrCeaseScheme" -> becomeOrCeaseScheme,
        "schemeChangeDate" -> Json.obj(
          "schemeChangeDate" -> s"${taxYear}-04-06"
        )
      ) ++ contractsOrPoliciesNode(becomeOrCeaseScheme, contractsOrPolicies)

      val ua = Json.obj(
        "event10" -> event10Details,
        "taxYear" -> taxYear
      )

      val startDateDetailsNode = Json.obj(
        "startDateOfInvReg" -> s"${taxYear}-04-06",
      ) ++ contractsOrPoliciesNode(becomeOrCeaseScheme, contractsOrPolicies)

      //startDateDetails node exists only if become is selected and ceaseDateDetails node exists only if ceases is selected.
      val expected = Json.obj(
        "invRegScheme" -> Json.obj(
          "startDateDetails" -> startDateDetailsNode
        )
      )
      Tuple2(ua, expected)
    }
  }

  /*
  "event11" : {
              "hasSchemeChangedRulesUnAuthPayments" : true,
              "unAuthPaymentsRuleChangeDate" : {
                  "date" : "2023-08-12"
              },
              "hasSchemeChangedRulesInvestmentsInAssets" : true,
              "investmentsInAssetsRuleChangeDate" : {
                  "date" : "2023-08-23"
              }
          }
      }
      "event11" : {
              "hasSchemeChangedRulesUnAuthPayments" : true,
              "unAuthPaymentsRuleChangeDate" : {
                  "date" : "2023-08-12"
              },
              "hasSchemeChangedRulesInvestmentsInAssets" : false
          }
      }
      "event11" : {
              "hasSchemeChangedRulesUnAuthPayments" : false,
              "hasSchemeChangedRulesInvestmentsInAssets" : true,
              "investmentsInAssetsRuleChangeDate" : {
                  "date" : "2023-08-23"
              }
          }
      }
      "event11" : {
              "hasSchemeChangedRulesUnAuthPayments" : false,
              "hasSchemeChangedRulesInvestmentsInAssets" : false
          }
      }
   */
  def generateUserAnswersAndPOSTBodyEvent11: Gen[(JsObject, JsObject)] = {
    for {
      taxYear <- taxYearGenerator
      hasSchemeChangedRulesUnAuthPayments <- arbitrary[Boolean]
      hasSchemeChangedRulesInvestmentsInAssets <- arbitrary[Boolean]
    } yield {
      def event11Details(unAuthPayments: Boolean, investmentsInAssets: Boolean): JsObject = {

        (unAuthPayments, investmentsInAssets) match {
          case (true, true) =>
            Json.obj(
              "hasSchemeChangedRulesUnAuthPayments" -> unAuthPayments,
              "unAuthPaymentsRuleChangeDate" -> Json.obj(
                "date" -> s"$taxYear-04-06"
              ),
              "hasSchemeChangedRulesInvestmentsInAssets" -> investmentsInAssets,
              "investmentsInAssetsRuleChangeDate" -> Json.obj(
                "date" -> s"$taxYear-04-30"
              )
            )
          case (false, true) =>
            Json.obj(
              "hasSchemeChangedRulesUnAuthPayments" -> unAuthPayments,
              "hasSchemeChangedRulesInvestmentsInAssets" -> investmentsInAssets,
              "investmentsInAssetsRuleChangeDate" -> Json.obj(
                "date" -> s"$taxYear-04-30"
              )
            )
          case (true, false) =>
            Json.obj(
              "hasSchemeChangedRulesUnAuthPayments" -> unAuthPayments,
              "unAuthPaymentsRuleChangeDate" -> Json.obj(
                "date" -> s"$taxYear-04-06"
              ),
              "hasSchemeChangedRulesInvestmentsInAssets" -> investmentsInAssets
            )
          case _ => Json.obj()
            Json.obj(
              "hasSchemeChangedRulesUnAuthPayments" -> unAuthPayments,
              "hasSchemeChangedRulesInvestmentsInAssets" -> investmentsInAssets
            )
        }

      }

      val ua = Json.obj(
        "event11" -> event11Details(hasSchemeChangedRulesUnAuthPayments, hasSchemeChangedRulesInvestmentsInAssets),
        "taxYear" -> taxYear
      )
      val expected = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"${taxYear.toInt + 1}-04-05"
        ),
        "eventDetails" -> Json.obj(
          "event11" -> Json.obj(
            "recordVersion" -> "001",

          )
        )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent12: Gen[(JsObject, JsObject)] = {
    for {
      taxYear <- taxYearGenerator
    } yield {
      val ua = Json.obj(
        "event12" -> Json.obj(
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
            "recordVersion" -> "001",
            "twoOrMoreSchemesDate" -> s"${taxYear}-04-06"
          )
        )
      )
      Tuple2(ua, expected)
    }
  }

  /*
  "taxYear" : "2023",
          "event13" : {
              "schemeStructure" : "single",
              "changeDate" : "2023-08-12"
          }
      }

      "taxYear" : "2023",
          "event13" : {
              "schemeStructure" : "other",
              "changeDate" : "2023-08-12",
              "schemeStructureDescription" : "Testing"
          }
      }
   */

  private val schemeStructureETMPEvent13: Map[String, String] = {
    Map("single" -> "A single trust under which all of the assets are held for the benefit of all members of the scheme",
      "group" -> "A group life/death in service scheme",
      "corporate" -> "A body corporate",
      "other" -> "Other")
  }

  def generateUserAnswersAndPOSTBodyEvent13: Gen[(JsObject, JsObject)] = {
    for {
      taxYear <- taxYearGenerator
      schemeStructureUA <- Gen.oneOf(schemeStructureETMPEvent13.keys.toSeq)
      schemeStructureDescription <- Gen.alphaStr
    } yield {
      def event13Details(keyOrValue: String): JsObject = {
        (keyOrValue, schemeStructureUA) match {
          case ("key", "other") => Json.obj(
            "recordVersion" -> "001",
            "schemeStructure" -> schemeStructureUA,
            "changeDate" -> s"$taxYear-04-06",
            "schemeStructureDescription" -> schemeStructureDescription
          )
          case ("key", _) => Json.obj(
            "recordVersion" -> "001",
            "schemeStructure" -> schemeStructureUA,
            "changeDate" -> s"$taxYear-04-06"
          )
          case ("value", "Other") => Json.obj(
            "recordVersion" -> "001",
            "schemeStructure" -> schemeStructureETMPEvent13(schemeStructureUA),
            "changeDate" -> s"$taxYear-04-06",
            "schemeStructureDescription" -> schemeStructureDescription
          )
          case _ => Json.obj(
            "recordVersion" -> "001",
            "schemeStructure" -> schemeStructureETMPEvent13(schemeStructureUA),
            "changeDate" -> s"$taxYear-04-06"
          )
        }
      }

      val ua = Json.obj(
        "event13" -> event13Details("key"),
        "taxYear" -> taxYear
      )
      val expected = Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> s"$taxYear-04-06",
          "reportEndDate" -> s"${taxYear.toInt + 1}-04-05"
        ),
        "eventDetails" -> Json.obj(
          "event13" -> event13Details("value")
        )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent14: Gen[(JsObject, JsObject)] = {
    for {
      taxYear <- taxYearGenerator
      schemeMembers <- Gen.oneOf(Seq("0", "1", "2 to 11", "12 to 50", "51 to 10,000", "More than 10,000"))
    } yield {
      val ua = Json.obj(
        "event14" -> Json.obj(
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
            "recordVersion" -> "001",
            "schemeMembers" -> schemeMembers
          )
        )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyWindUp: Gen[(JsObject, JsObject)] = {
    for {
      schemeWindUpDate <- dateGenerator
      taxYear <- taxYearGenerator
    } yield {
      val endTaxYear = (taxYear.toInt + 1).toString
      val fullUA = Json.obj(
        "eventWindUp" -> Json.obj(
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
            "recordVersion" -> "001",
            "dateOfWindUp" -> schemeWindUpDate
          )
        )
      )
      Tuple2(fullUA, fullExpectedResult)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent18: Gen[(JsObject, JsObject)] = {
    for {
      taxYear <- taxYearGenerator
    } yield {
      val endTaxYear = (taxYear.toInt + 1).toString
      val fullUA = Json.obj(
        "event18" -> Json.obj(
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
            "recordVersion" -> "001",
            "chargeablePmt" -> "Yes"
          )
        )
      )
      Tuple2(fullUA, fullExpectedResult)
    }
  }
}