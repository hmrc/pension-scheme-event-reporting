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

    def eventReportDetailsNode(x: JsObject) = Json.obj(
      "eventDetails" -> x,
      "eventReportDetails" -> Json.obj(
        "reportStartDate" -> "2020-09-01",
        "reportEndDate" -> "2020-09-01"
      )
    )

    val y = (__ \ "schemeWindUpDate").readNullable[String].map {
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
    val x = (__ \ "event18Confirmation").readNullable[Boolean].map {
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
    x.flatMap { v =>
      y.map {
        c =>
          (v, c) match {
            case (Some(valueA), Some(valueB)) =>
              Some(eventReportDetailsNode(valueA ++ valueB))
            case (Some(valueA), None) => Some(eventReportDetailsNode(valueA))
            case (None, Some(valueB)) => Some(eventReportDetailsNode(valueB))
            case (None, None) => None
          }
      }
    }
  }
}

