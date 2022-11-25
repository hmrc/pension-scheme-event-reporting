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
    (__ \ "schemeWindUpDate").readNullable[String].map {
      case Some(date) =>
        Some(
          Json.obj(
            "eventDetails" ->
              Json.obj(
                "eventWindUp" -> Json.obj(
                  "dateOfWindUp" -> date
                )
              ),
            "eventReportDetails" -> Json.obj(
              "reportStartDate" -> "2020-09-01",
              "reportEndDate" -> "2020-09-01"
            )
          )
        )
      case _ => None
    }
  }
}
