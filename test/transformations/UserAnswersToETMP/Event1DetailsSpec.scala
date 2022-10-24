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
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}
import utils.{JsonFileReader, ResponseGenerators}

class Event1DetailsSpec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader with ResponseGenerators with ScalaCheckPropertyChecks {

  "readsWhoReceivedUnauthorisedPayment" - {
    "work correctly for valid payload for member" in {
      val inputJson = Json.obj(
        "whoReceivedUnauthPayment" -> "member"
      )
      val result = inputJson.transform(Event1Details.readsWhoReceivedUnauthorisedPayment)
      result mustBe JsSuccess(Json.obj(
        "memberType" -> "Individual"
      ))
    }

    "work correctly for valid payload for employer" in {
      val inputJson = Json.obj(
        "whoReceivedUnauthPayment" -> "employer"
      )
      val result = inputJson.transform(Event1Details.readsWhoReceivedUnauthorisedPayment)
      result mustBe JsSuccess(Json.obj(
        "memberType" -> "Employer"
      ))
    }

    "fail for invalid payload" in {
      val inputJson = Json.obj(
        "whoReceivedUnauthPayment" -> "unknown"
      )
      val result = inputJson.transform(Event1Details.readsWhoReceivedUnauthorisedPayment)
      result.isError mustBe true
    }
  }

  "transformToETMPData" - {
    "must transform a randomly generated valid payload correctly" in {
      forAll(generateRandomPayloadAPI1827) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(Event1Details.transformToETMPData)
          val expectedResult = JsSuccess(expectedResponse)
          println(s"\n\n ------- GENERATED USER ANSWERS:  $userAnswers")
          println(s"\n\n ------- GENERATED EXPECTED:   $expectedResult")
          println(s"\n\n ------- ACTUAL RESULT:  $result")
          result.asOpt mustBe expectedResult.asOpt
      }
      //      //    {
      //      //    "transform a valid payload correctly when read from sample file" in {
      //      //      val json = readJsonFromFile("/api-1834-valid-example.json")
      //      //      val result = json.validate(EventSummary.rds).asOpt
      //      //
      //      //      val expectedResult = Some(
      //      //        Json.arr("10", "11", "12", "13", "14", "19", "20", "0")
      //      //      )
      //      //
      //      //      result mustBe expectedResult
      //      //    }
      //
      //    }
    }
  }


}

