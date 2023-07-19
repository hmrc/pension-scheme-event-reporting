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
import utils.{GeneratorAPI1832, GeneratorAPI1834Summary, JsonFileReader}

class API1834SummarySpec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader
  with GeneratorAPI1834Summary with GeneratorAPI1832 with ScalaCheckPropertyChecks {

  "Reads" - {
    //    "transform a valid payload correctly when read from sample file from API 1834" in {
    //      val json = readJsonFromFile("/api-1834-valid-example.json")
    //      val result = json.validate(API1834Summary.rdsFor1834).asOpt
    //
    //      val expectedResult = Some(
    //        Seq("1","2","3","4","5","6","7","8","8A","10","11","12","13","14","19","20","22","23","WindUp")
    //      )
    //
    //      result.map(_.validate[Seq[String]].get) mustBe expectedResult
    //    }
    //
    //    "transform a randomly generated API 1834 events valid payload correctly" in {
    //      forAll(generateGET1834ResponseAndUserAnswers) {
    //        case (json: JsObject, eventTypes: Seq[String]) =>
    //          val result = json.validate(API1834Summary.rdsFor1834).asOpt
    //          val expectedResult = Some(eventTypes)
    //          result.map(_.validate[Seq[String]].get) mustBe expectedResult
    //      }
    //    }
    //
    //    "transform a valid payload correctly when read from sample file from API 1831" in {
    //      val json = readJsonFromFile("/api-1831-valid-example.json")
    //      val result = json.validate(API1834Summary.rdsFor1831).asOpt
    //      val expectedResult = Some(Seq("20A"))
    //      result.map(_.validate[Seq[String]].get) mustBe expectedResult
    //    }
    //
    //    "transform a randomly generated API 1831 events valid payload correctly" in {
    //      val generatedPayload = Json.obj("er20aDetails" -> Json.obj("reportVersionNumber" -> "001"))
    //      val result = generatedPayload.validate(API1834Summary.rdsFor1831).asOpt
    //      val expectedResult = Some(Seq("20A"))
    //      result.map(_.validate[Seq[String]].get) mustBe expectedResult
    //    }
    "transform a randomly generated API 1834 events valid payload correctly with different recordVersions" in {

      val generatedPayload = Json.obj(
        "event1ChargeDetails" -> Json.obj(
          "recordVersion" -> "002"
        ),
        "memberEventsSummary" -> Json.obj(
          "event2" -> Json.obj("recordVersion" -> "001"),
          "event3" -> Json.obj("recordVersion" -> "002")
        ),
        "eventDetails" -> Json.obj(
          "event10" -> Json.obj("recordVersion" -> "002"),
          "event11" -> Json.obj("recordVersion" -> "001"),
        )
      )

      val result = generatedPayload.validate(API1834Summary.rdsFor1834)
      val expectedResult = JsSuccess(
        JsArray(
          Seq(
            Json.obj("eventType" -> "event1", "recordVersion" -> 2),
            Json.obj("eventType" -> "event2", "recordVersion" -> 1),
            Json.obj("eventType" -> "event3", "recordVersion" -> 2),
            Json.obj("eventType" -> "event10", "recordVersion" -> 2),
            Json.obj("eventType" -> "event11", "recordVersion" -> 1)
          )
        )
      )
      result mustBe expectedResult
    }
  }
}
