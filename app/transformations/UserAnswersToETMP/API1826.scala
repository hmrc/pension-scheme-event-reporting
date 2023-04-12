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

object API1826 extends Transformer {

  val transformToETMPData: Reads[JsObject] = {

    def eventTypeNodes(events: Seq[JsObject]): JsObject = {
      val eventDetailNodes = events.foldLeft(Json.obj())((a, b) => a ++ b)
      if (events.isEmpty) Json.obj() else Json.obj("eventDetails" -> eventDetailNodes)
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

    val event13 = (__ \ "event13").readNullable[JsArray].map { optJsonArray =>
      optJsonArray.map { jsonArray =>
        Json.obj(
          "event13" -> jsonArray.value.map { json =>
            Json.obj(
              "recordVersion" -> (json \ "recordVersion").asOpt[String],
              "schemeStructure" -> (json \ "schemeStructure").as[String],
              "schemeStructureOther" -> (json \ "schemeStructureOther").asOpt[String],
              "dateOfChange" -> (json \ "dateOfChange").as[String]
            )
          }
        )
      }
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
      ev13 <- event13
      ev18 <- event18
      schWindUp <- schemeWindUp
      header <- HeaderForAllAPIs.transformToETMPData()
    } yield {
      header ++ eventTypeNodes((ev13 ++ ev18 ++ schWindUp).toSeq)
    }
  }
}
