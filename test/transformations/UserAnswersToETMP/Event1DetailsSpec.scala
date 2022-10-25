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

package transformations.UserAnswersToETMP

import org.mockito.MockitoSugar
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{JsonFileReader, ResponseGenerators}

class Event1DetailsSpec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader with ResponseGenerators with ScalaCheckPropertyChecks {

  "transformToETMPData" - {
    "must transform a randomly generated valid payload correctly" in {
      forAll(generateRandomPayloadAPI1827) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(Event1Details.transformToETMPData)
          val expectedResult = JsSuccess(expectedResponse, __ \ 'membersOrEmployers)
          println(s"\n\n ------- GENERATED USER ANSWERS:  $userAnswers")
          println(s"\n\n ------- GENERATED EXPECTED:   $expectedResult")
          println(s"\n\n ------- ACTUAL RESULT:  $result")
          result mustBe expectedResult
      }
    }

    "must transform a valid payload correctly when read from sample file" in {
      val json = readJsonFromFile("/api-1827-valid-example.json")
      val result = json.validate(Event1Details.transformToETMPData)

      val expectedResult = JsSuccess(
        Json.parse(
          """
            |{
            |   "eventReportDetails":{
            |      "reportStartDate":"2020-09-01",
            |      "reportEndDate":"2020-09-01"
            |   },
            |   "event1Details":{
            |      "event1Details":[
            |         {
            |            "unAuthorisedPaymentDetails":{
            |               "freeTxtOrSchemeOrRecipientName":"bik",
            |               "dateOfUnauthorisedPayment":"2022-09-22",
            |               "unAuthorisedPmtType1":"Benefit in kind",
            |               "valueOfUnauthorisedPayment":2000.6
            |            },
            |            "individualMemberDetails":{
            |               "firstName":"maryiah",
            |               "lastName":"m",
            |               "pmtMoreThan25PerFundValue":"No",
            |               "nino":"AB123456C",
            |               "signedMandate":"Yes"
            |            },
            |            "memberType":"Individual"
            |         }
            |      ]
            |   }
            |}
            |""".stripMargin),
        __ \ 'membersOrEmployers
      )

      result mustBe expectedResult
    }
  }
}

