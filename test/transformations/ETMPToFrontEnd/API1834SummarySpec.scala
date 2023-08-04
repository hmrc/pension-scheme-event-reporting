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
import play.api.libs.json.{Json, _}
import utils.{GeneratorAPI1832, GeneratorAPI1834Summary, JsonFileReader}

class API1834SummarySpec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader
  with GeneratorAPI1834Summary with GeneratorAPI1832 with ScalaCheckPropertyChecks {

  "Reads" - {
    "transform a valid payload correctly when read from sample file from API 1831" in {
      val json = readJsonFromFile("/api-1831-valid-example.json")
      val result = json.validate(API1834Summary.rdsFor1831)
      val expectedResult = JsSuccess(
        JsArray(
          Seq(
            Json.obj("eventType" -> "20A", "recordVersion" -> 1)
          )
        ),
        __ \ "er20aDetails" \ "reportVersionNumber"
      )


      result mustBe expectedResult
    }

    "transform an API 1831 events valid payload correctly" in {
      val generatedPayload = Json.obj("er20aDetails" -> Json.obj("reportVersionNumber" -> "002"))
      val result = generatedPayload.validate(API1834Summary.rdsFor1831)
      val expectedResult = JsSuccess(
        JsArray(
          Seq(
            Json.obj("eventType" -> "20A", "recordVersion" -> 2)
          )
        ),
        __ \ "er20aDetails" \ "reportVersionNumber"
      )
      result mustBe expectedResult
    }

    "transform an API 1834 events valid payload correctly with different recordVersions" in {

      val generatedPayload = Json.obj(
        "event1ChargeDetails" -> Json.obj(
          "recordVersion" -> "002",
          "numberOfMembers" -> 1
        ),
        "memberEventsSummary" -> Json.obj(
          "event2" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event3" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event4" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event5" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event6" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event7" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event8" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event8A" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event22" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event23" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
        ),
        "eventDetails" -> Json.obj(
          "event10" -> Json.arr(Json.obj("recordVersion" -> "002")), // array
          "event11" -> Json.obj("recordVersion" -> "001"),
          "event12" -> Json.obj("recordVersion" -> "001"),
          "event13" -> Json.arr(Json.obj("recordVersion" -> "002")), // array
          "event14" -> Json.obj("recordVersion" -> "001"),
          "event18" -> Json.obj("recordVersion" -> "002"),
          "event19" -> Json.arr(Json.obj("recordVersion" -> "001")), // array
          "event20" -> Json.arr(Json.obj("recordVersion" -> "002")) // array
        )
      )

      val result = generatedPayload.validate(API1834Summary.rdsFor1834)
      val expectedResult = JsSuccess(
        JsArray(
          Seq(
            Json.obj("eventType" -> "1", "recordVersion" -> 2),
            Json.obj("eventType" -> "2", "recordVersion" -> 1),
            Json.obj("eventType" -> "3", "recordVersion" -> 2),
            Json.obj("eventType" -> "4", "recordVersion" -> 1),
            Json.obj("eventType" -> "5", "recordVersion" -> 2),
            Json.obj("eventType" -> "6", "recordVersion" -> 1),
            Json.obj("eventType" -> "7", "recordVersion" -> 2),
            Json.obj("eventType" -> "8", "recordVersion" -> 1),
            Json.obj("eventType" -> "8A", "recordVersion" -> 2),
            Json.obj("eventType" -> "22", "recordVersion" -> 1),
            Json.obj("eventType" -> "23", "recordVersion" -> 2),
            Json.obj("eventType" -> "10", "recordVersion" -> 2),
            Json.obj("eventType" -> "11", "recordVersion" -> 1),
            Json.obj("eventType" -> "12", "recordVersion" -> 1),
            Json.obj("eventType" -> "13", "recordVersion" -> 2),
            Json.obj("eventType" -> "14", "recordVersion" -> 1),
            Json.obj("eventType" -> "18", "recordVersion" -> 2),
            Json.obj("eventType" -> "19", "recordVersion" -> 1),
            Json.obj("eventType" -> "20", "recordVersion" -> 2)
          )
        )
      )
      result mustBe expectedResult
    }


    "transform when array types missing" in {

      val generatedPayload = Json.obj(
        "event1ChargeDetails" -> Json.obj(
          "recordVersion" -> "002",
          "numberOfMembers" -> 1
        ),
        "memberEventsSummary" -> Json.obj(
          "event2" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event3" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event4" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event5" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event6" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event7" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event8" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event8A" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event22" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event23" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
        ),
        "eventDetails" -> Json.obj(
          "event11" -> Json.obj("recordVersion" -> "001"),
          "event12" -> Json.obj("recordVersion" -> "001"),
          "event14" -> Json.obj("recordVersion" -> "001"),
          "event18" -> Json.obj("recordVersion" -> "002")
        )
      )

      val result = generatedPayload.validate(API1834Summary.rdsFor1834)
      val expectedResult = JsSuccess(
        JsArray(
          Seq(
            Json.obj("eventType" -> "1", "recordVersion" -> 2),
            Json.obj("eventType" -> "2", "recordVersion" -> 1),
            Json.obj("eventType" -> "3", "recordVersion" -> 2),
            Json.obj("eventType" -> "4", "recordVersion" -> 1),
            Json.obj("eventType" -> "5", "recordVersion" -> 2),
            Json.obj("eventType" -> "6", "recordVersion" -> 1),
            Json.obj("eventType" -> "7", "recordVersion" -> 2),
            Json.obj("eventType" -> "8", "recordVersion" -> 1),
            Json.obj("eventType" -> "8A", "recordVersion" -> 2),
            Json.obj("eventType" -> "22", "recordVersion" -> 1),
            Json.obj("eventType" -> "23", "recordVersion" -> 2),
            Json.obj("eventType" -> "11", "recordVersion" -> 1),
            Json.obj("eventType" -> "12", "recordVersion" -> 1),
            Json.obj("eventType" -> "14", "recordVersion" -> 1),
            Json.obj("eventType" -> "18", "recordVersion" -> 2)
          )
        )
      )
      result mustBe expectedResult
    }

    "transform an API 1834 events valid members payload correctly when numberOfMembers is zero" in {
      val generatedPayload = Json.obj(
        "event1ChargeDetails" -> Json.obj(
          "recordVersion" -> "002", "numberOfMembers" -> 0
        ),
        "memberEventsSummary" -> Json.obj(
          "event2" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 0),
          "event3" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 0),
          "event4" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 0),
          "event5" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 0),
          "event6" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 0),
          "event7" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 0),
          "event8" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 0),
          "event8A" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 0),
          "event22" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 0),
          "event23" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 0),
        )
      )

      val result = generatedPayload.validate(API1834Summary.rdsFor1834)
      val expectedResult = JsSuccess(
        JsArray(Nil)
      )
      result mustBe expectedResult
    }


    "transform an API 1834 events valid payload correctly with different recordVersions but no event 1" in {

      val generatedPayload = Json.obj(
        "memberEventsSummary" -> Json.obj(
          "event2" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event3" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event4" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event5" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event6" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event7" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event8" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event8A" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
          "event22" -> Json.obj("recordVersion" -> "001", "numberOfMembers" -> 1),
          "event23" -> Json.obj("recordVersion" -> "002", "numberOfMembers" -> 1),
        ),
        "eventDetails" -> Json.obj(
          "event10" -> Json.arr(Json.obj("recordVersion" -> "002")), // array
          "event11" -> Json.obj("recordVersion" -> "001"),
          "event12" -> Json.obj("recordVersion" -> "001"),
          "event13" -> Json.arr(Json.obj("recordVersion" -> "002")), // array
          "event14" -> Json.obj("recordVersion" -> "001"),
          "event18" -> Json.obj("recordVersion" -> "002"),
          "event19" -> Json.arr(Json.obj("recordVersion" -> "001")), // array
          "event20" -> Json.arr(Json.obj("recordVersion" -> "002")) // array
        )
      )

      val result = generatedPayload.validate(API1834Summary.rdsFor1834)
      val expectedResult = JsSuccess(
        JsArray(
          Seq(
            Json.obj("eventType" -> "2", "recordVersion" -> 1),
            Json.obj("eventType" -> "3", "recordVersion" -> 2),
            Json.obj("eventType" -> "4", "recordVersion" -> 1),
            Json.obj("eventType" -> "5", "recordVersion" -> 2),
            Json.obj("eventType" -> "6", "recordVersion" -> 1),
            Json.obj("eventType" -> "7", "recordVersion" -> 2),
            Json.obj("eventType" -> "8", "recordVersion" -> 1),
            Json.obj("eventType" -> "8A", "recordVersion" -> 2),
            Json.obj("eventType" -> "22", "recordVersion" -> 1),
            Json.obj("eventType" -> "23", "recordVersion" -> 2),
            Json.obj("eventType" -> "10", "recordVersion" -> 2),
            Json.obj("eventType" -> "11", "recordVersion" -> 1),
            Json.obj("eventType" -> "12", "recordVersion" -> 1),
            Json.obj("eventType" -> "13", "recordVersion" -> 2),
            Json.obj("eventType" -> "14", "recordVersion" -> 1),
            Json.obj("eventType" -> "18", "recordVersion" -> 2),
            Json.obj("eventType" -> "19", "recordVersion" -> 1),
            Json.obj("eventType" -> "20", "recordVersion" -> 2)
          )
        )
      )
      result mustBe expectedResult
    }

  }
}
