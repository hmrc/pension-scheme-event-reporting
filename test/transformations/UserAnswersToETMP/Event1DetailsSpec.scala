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
import play.api.libs.json.JsObject
import utils.{JsonFileReader, ResponseGenerators}

class Event1DetailsSpec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader with ResponseGenerators with ScalaCheckPropertyChecks {

  "Reads" - {
    "must transform a randomly generated valid payload correctly" in {
      forAll(generateRandomPayloadAPI1827) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(Event1Details.transformToETMPData).asOpt
          val expectedResult = Some(expectedResponse)
          println(s"\n\n ------- RESULT:  $result")
          println(s"\n\n ------- EXPECTED:   $expectedResult")
          result mustBe expectedResult
      }
      //    {
      //    "transform a valid payload correctly when read from sample file" in {
      //      val json = readJsonFromFile("/api-1834-valid-example.json")
      //      val result = json.validate(EventSummary.rds).asOpt
      //
      //      val expectedResult = Some(
      //        Json.arr("10", "11", "12", "13", "14", "19", "20", "0")
      //      )
      //
      //      result mustBe expectedResult
      //    }

    }
  }
}

