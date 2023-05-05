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

import models.enumeration.EventType
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1832, GeneratorAPI1834, JsonFileReader}

class EventSummarySpec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader
  with GeneratorAPI1834 with GeneratorAPI1832 with ScalaCheckPropertyChecks {

  "Reads" - {
    "transform a valid payload correctly when read from sample file from API 1834" in {
      val json = readJsonFromFile("/api-1834-valid-example.json")
      val result = json.validate(EventSummary.rdsFor1834).asOpt

      val expectedResult = Some(
        Set("2","3","4","5","6","7","8","8A","10","11","12","13","14","19","20","22","23","24","0")
      )

      result.map(_.validate[Set[String]].get) mustBe expectedResult
    }

    "transform a valid payload correctly when read from sample file from API 1832" in {
      val json = readJsonFromFile("/api-1832-valid-example.json")
      val result = json.validate(EventSummary.rdsEventTypeNodeOnly(EventType.Event22)).asOpt

      val expectedResult = Some(
        Json.arr("22")
      )

      result mustBe expectedResult
    }

    "transform a randomly generated API 1834 events valid payload correctly" in {
      forAll(generateGET1834ResponseAndUserAnswers) {
        case (json: JsObject, eventTypes: Seq[String]) =>
          val result = json.validate(EventSummary.rdsFor1834).asOpt
          val expectedResult = Some(eventTypes.toSet)
          result.map(_.validate[Seq[String]].get.toSet) mustBe expectedResult
      }
    }
  }
}
