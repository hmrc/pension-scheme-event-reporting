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

package transformations.UserAnswersToETMP

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1826, JsonFileReader}

class API1826Spec extends AnyFreeSpec with Matchers
  with JsonFileReader with GeneratorAPI1826 with ScalaCheckPropertyChecks {

  "transformToETMPData" - {
    "must transform a randomly generated valid payload correctly for Event 10" in {
      forAll(generateUserAnswersAndPOSTBodyEvent10) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1826.transformToETMPData)
          val expectedResult = JsSuccess(expectedResponse, __ \ Symbol("event10"))
          result mustBe expectedResult
      }
    }
    "must transform a randomly generated valid payload correctly for Event 11" in {
      forAll(generateUserAnswersAndPOSTBodyEvent11) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          if (userAnswers == expectedResponse) {
            Right("Test bypassed, transformation not required when both booleans are false")
          } else {
            val result = userAnswers.validate(API1826.transformToETMPData)
            val expectedResult = JsSuccess(expectedResponse)
            result mustBe expectedResult
          }
      }
    }
    "must transform a randomly generated valid payload correctly for Event 12" in {
      forAll(generateUserAnswersAndPOSTBodyEvent12) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1826.transformToETMPData)
          val expectedResult = JsSuccess(expectedResponse)
          result mustBe expectedResult
      }
    }
    //    "must transform a randomly generated valid payload correctly for Event 13" in {
    //      forAll(generateUserAnswersAndPOSTBodyEvent13) {
    //        case (userAnswers: JsObject, expectedResponse: JsObject) =>
    //          val result = userAnswers.validate(API1826.transformToETMPData)
    //          val expectedResult = JsSuccess(expectedResponse)
    //          result mustBe expectedResult
    //      }
    //    }
    "must transform a randomly generated valid payload correctly for Event 14" in {
      forAll(generateUserAnswersAndPOSTBodyEvent14) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1826.transformToETMPData)
          val expectedResult = JsSuccess(expectedResponse)
          result mustBe expectedResult
      }
    }
    "must transform a randomly generated valid payload correctly for Wind Up" in {
      forAll(generateUserAnswersAndPOSTBodyWindUp) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1826.transformToETMPData)
          val expectedResult = JsSuccess(expectedResponse)
          result mustBe expectedResult
      }
    }
    "must transform a randomly generated valid payload correctly for Event 18" in {
      forAll(generateUserAnswersAndPOSTBodyEvent18) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1826.transformToETMPData)
          result.asOpt mustBe Some(expectedResponse)
      }
    }

    //    "must transform all events when present" in {
    //
    //      val event10Obj = Json.obj(
    //        "recordVersion" -> "001",
    //        "invRegScheme" -> Json.obj(
    //          "startDateDetails" -> Json.obj(
    //            "startDateOfInvReg" -> "2022-01-31",
    //            "contractsOrPolicies" -> "Yes"
    //          )
    //        )
    //      )
    //      val event10 = JsArray(Seq(
    //        event10Obj,
    //        event10Obj
    //      ))
    //
    //      val event11ObjUA = Json.parse(
    //        """
    //          |{
    //          |            "recordVersion": "001",
    //          |            "hasSchemeChangedRulesUnAuthPayments" : true,
    //          |            "unAuthPaymentsRuleChangeDate" : {
    //          |                "date" : "2022-01-31"
    //          |            },
    //          |            "hasSchemeChangedRulesInvestmentsInAssets" : true,
    //          |            "investmentsInAssetsRuleChangeDate" : {
    //          |                "date" : "2022-01-10"
    //          |            }
    //          |        }
    //          |""".stripMargin
    //      )
    //
    //      val event11Obj = Json.parse(
    //        """
    //          |{
    //          |      "recordVersion": "001",
    //          |      "unauthorisedPmtsDate": "2022-01-31",
    //          |      "contractsOrPoliciesDate": "2022-01-10"
    //          |    }
    //          |""".stripMargin
    //      )
    //
    //      val event12Obj = Json.parse(
    //        """{
    //          |      "twoOrMoreSchemesDate": "2022-01-02"
    //          |    }""".stripMargin)
    //
    //      val event12ObjUA = Json.parse(
    //        """{
    //          |      "dateOfChange": {
    //          |          "dateOfChange": "2022-01-02"
    //          |        }
    //          |    }""".stripMargin)
    //
    //      val event13Obj = Json.obj(
    //        "recordVersion" -> "001",
    //        "schemeStructure" -> "A single trust under which all of the assets are held for the benefit of all members of the scheme",
    //        "schemeStructureOther" -> "Text",
    //        "dateOfChange" -> "2022-03-23"
    //      )
    //      val event13 = JsArray(Seq(
    //        event13Obj,
    //        event13Obj
    //      ))
    //
    //      val event14Obj = Json.parse(
    //        """{
    //          |      "recordVersion": "001",
    //          |      "schemeMembers": "12 to 50"
    //          |    }""".stripMargin)
    //
    //      val event19Obj = Json.parse(
    //        """{
    //          |        "recordVersion": "001",
    //          |        "countryCode": "GB",
    //          |        "dateOfChange": "2022-01-14"
    //          |      }""".stripMargin)
    //
    //      val event19 = JsArray(Seq(
    //        event19Obj,
    //        event19Obj
    //      ))
    //
    //      val event20Obj = Json.parse(
    //        """{
    //          |        "recordVersion": "001",
    //          |        "occSchemeDetails": {
    //          |          "startDateOfOccScheme": "2022-01-27"
    //          |        }
    //          |      }""".stripMargin)
    //
    //      val event20 = JsArray(Seq(
    //        event20Obj,
    //        event20Obj
    //      ))
    //
    //      val userAnswers: JsObject = {
    //        Json.obj(
    //          "event18Confirmation" -> true,
    //          "schemeWindUpDate" -> "1991-11-22",
    //          "taxYear" -> "2020",
    //          "event10" -> event10,
    //          "event11" -> event11ObjUA,
    //          "event12" -> event12ObjUA,
    //          "event13" -> event13,
    //          "event14" -> event14Obj,
    //          "event19" -> event19,
    //          "event20" -> event20
    //        )
    //      }
    //      val expected: JsObject = {
    //        Json.obj(
    //          "eventReportDetails" -> Json.obj(
    //            "reportStartDate" -> "2020-04-06",
    //            "reportEndDate" -> "2021-04-05"
    //          ),
    //          "eventDetails" -> Json.obj(
    //            "event10" -> event10,
    //            "event11" -> event11Obj,
    //            "event12" -> event12Obj,
    //            "event13" -> event13,
    //            "event14" -> event14Obj,
    //            "event18" -> Json.obj(
    //              "chargeablePmt" -> "Yes"
    //            ),
    //            "event19" -> event19,
    //            "event20" -> event20,
    //            "eventWindUp" -> Json.obj(
    //              "dateOfWindUp" -> "1991-11-22"
    //            )
    //          )
    //        )
    //      }
    //      val result = userAnswers.validate(API1826.transformToETMPData)
    //      result.asOpt mustBe Some(expected)
    //    }

    "must not transform an event that is not present" in {
      val userAnswers: JsObject =
        Json.obj("taxYear" -> "2020")

      val result = userAnswers.validate(API1826.transformToETMPData)
      val expected: JsObject = {
        Json.obj(
          "eventReportDetails" -> Json.obj(
            "reportStartDate" -> "2020-04-06",
            "reportEndDate" -> "2021-04-05"
          )
        )
      }
      result.asOpt mustBe Some(expected)
    }
  }
}

