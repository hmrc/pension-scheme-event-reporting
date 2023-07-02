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

import models.enumeration.EventType.{Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, WindUp}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1834, JsonFileReader}

class API1834Spec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader
  with GeneratorAPI1834 with ScalaCheckPropertyChecks {

  "Reads" - {
    //Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, WindUp
    val api1834Events = List(Event20)
    api1834Events.foreach { eventType =>
      s"transform a randomly generated valid payload from API 1834 correctly (Event ${eventType.toString})" in {
        forAll(generateUserAnswersAndPOSTBodyByEvent(eventType)) {
          case (payload: JsObject, expectedResponse: JsObject) =>
            println("\npayload = " + payload)
            println("\nexp ua = " + expectedResponse)
            val result = payload.validate(API1834.reads(eventType))
            println("\nactual = " + result)
            result.asOpt mustBe Some(expectedResponse)

        }
      }
    }
  }
}
