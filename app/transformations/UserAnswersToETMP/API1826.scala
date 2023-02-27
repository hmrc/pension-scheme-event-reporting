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

package transformations.UserAnswersToETMP

import play.api.libs.json._
import transformations.Transformer

object API1828Spec extends Transformer {
  val transformToETMPData: Reads[JsObject] = {

    def eventReportDetailsNode(events: JsObject) = {
      val header = Json.obj("eventReportDetails" -> Json.obj(
        "reportStartDate" -> "2020-09-01",
        "reportEndDate" -> "2020-09-01"
      ))
      header ++ events
    }

    def eventTypeNodes(events: Seq[JsObject]): JsObject = {
      val eventDetailNodes = events.foldLeft(Json.obj())((a, b) => a ++ b)
      val eventJsObj = if (events.isEmpty) Json.obj() else Json.obj("eventDetails" -> eventDetailNodes)
      eventReportDetailsNode(eventJsObj)
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
      eventTypeNodes((ev18 ++ schWindUp).toSeq)
    }
  }
}

