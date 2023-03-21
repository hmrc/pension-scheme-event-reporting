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

package transformations.ETMPToFrontEnd

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1832, GeneratorAPI1834, JsonFileReader}

class MemberEventReportSpec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader
  with GeneratorAPI1834 with GeneratorAPI1832 with ScalaCheckPropertyChecks {

  "Reads" - {
    "transform a valid payload correctly when read from sample file from API 1834" in {
      val json = readJsonFromFile("/api-1832-valid-example.json")
      val result = json.validate(MemberEventReport.rds1832Api).asOpt

      val expectedResult =
        Json.obj(
          "event22" -> Json.obj(
            "members" -> Json.arr(
              Json.obj(
                "membersDetails"-> Json.obj(
                  "lastName" -> "Smith",
                  "firstName" -> "John",
                  "nino" -> "AA345678B"
                ),
                "chooseTaxYear" -> "2020",
                "totalPensionAmounts" -> 123.99
              )
            )
          )
        )

      result mustBe Some(expectedResult)
    }

    "transform a randomly generated valid payload from API 1832 correctly" in {
      forAll(generateGET1832UserAnswersFromETMP) {
        case (payload: JsObject, expectedResponse: JsObject) =>
          val result = payload.validate(MemberEventReport.rds1832Api).asOpt
          result mustBe Some(expectedResponse)

      }
    }
  }
}
