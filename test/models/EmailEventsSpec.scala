/*
 * Copyright 2024 HM Revenue & Customs
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

package models

import org.joda.time.DateTime
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsValue, Json}

class EmailEventsSpec extends AnyWordSpec with OptionValues with Matchers {

  import EmailEventsSpec._

  "Email Event" must {
    "format " when {
      "reading from json" in {
        val result = Json.fromJson[EmailEvents](emailResponseJson).asOpt.value
        result mustBe emailResponseEvents
      }

      "writing to json" in {
        val result = Json.toJson[EmailEvents](emailResponseEvents)
        result mustBe emailResponseJson
      }
    }
  }
}

object EmailEventsSpec {
  private val emailResponseJson: JsValue = Json.parse(
    """{
      |    "events": [
      |        {
      |            "event": "Sent",
      |            "detected": "2015-07-02T08:26:39.035Z"
      |        },
      |        {
      |            "event": "Delivered",
      |            "detected": "2015-07-02T08:25:20.068Z"
      |        }
      |    ]
      |}""".stripMargin
  )

  private val emailResponseEvents = EmailEvents(Seq(
    EmailEvent(Sent, DateTime.parse("2015-07-02T08:26:39.035Z")),
    EmailEvent(Delivered, DateTime.parse("2015-07-02T08:25:20.068Z"))
  ))
}
