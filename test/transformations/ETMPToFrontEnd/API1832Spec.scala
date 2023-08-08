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

import models.enumeration.EventType.{Event2, Event22, Event23, Event3, Event4, Event5, Event6, Event7, Event8, Event8A}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1832, JsonFileReader}

class API1832Spec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader
  with GeneratorAPI1832 with ScalaCheckPropertyChecks {

  "Reads" - {

    val api1832Events = List(Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A, Event22, Event23)

    api1832Events.foreach(
      event =>
        s"transform a randomly generated valid payload from API 1832 correctly (Event ${event.toString})" in {
          forAll(generateUserAnswersAndPOSTBodyByEvent(event)) {
            case (payload: JsObject, expectedResponse: JsObject) =>
              val result = payload.validate(API1832.rds1832Api(event))
              result mustBe JsSuccess(expectedResponse)
          }
        }
    )

    "transform a payload with no eventDetails node correctly as an empty json object" in {
      val payload = {
        val (json: JsObject, _) = {
          generateUserAnswersAndPOSTBodyByEvent(Event4).sample.get
        }
        json - "eventDetails"
      }

      val expectedResponse: JsObject = Json.obj()
      val result = payload.validate(API1832.rds1832Api(Event4)).asOpt
      result mustBe Some(expectedResponse)
    }
  }
}
