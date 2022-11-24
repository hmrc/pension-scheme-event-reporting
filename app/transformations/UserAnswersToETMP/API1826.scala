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

import play.api.libs.json._
import transformations.Transformer

object API1826 extends Transformer {
  val transformToETMPData: Reads[Option[JsObject]] = {

    def eventReportDetailsNode(events: JsObject) = Json.obj(
      "eventDetails" -> events,
      "eventReportDetails" -> Json.obj(
        "reportStartDate" -> "2020-09-01",
        "reportEndDate" -> "2020-09-01"
      )
    )

    def eventTypeNodes(event18:  Option[JsObject], schWindUp:  Option[JsObject]):  Option[JsObject] = {
      val eventTypeNodes = (event18 ++ schWindUp).toSeq.foldLeft(Json.obj())((a,b) => a ++ b)
      if (eventTypeNodes == Json.obj()) {
        None
      } else {
        Some(eventReportDetailsNode(eventTypeNodes))
      }
    }

    val schemeWindUp = (__ \ "schemeWindUpDate").readNullable[String].map {
      case Some(date) =>
        Some(
          Json.obj(
            "eventWindUp" -> Json.obj(
              "dateOfWindUp" -> date
            )
          )
        )
      case _ => None
    }

    val event18 = (__ \ "event18Confirmation").readNullable[Boolean].map {
      case Some(true) =>
        Some(
          Json.obj(
            "event18" -> Json.obj(
              "chargeablePmt" -> yes
            )
          )
        )
      case _ => None
    }

    for {
      ev18 <- event18
      schWindUp <- schemeWindUp
    } yield {
     eventTypeNodes(ev18, schWindUp)
    }
  }
}

