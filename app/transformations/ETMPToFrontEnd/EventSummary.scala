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

package transformations.ETMPToFrontEnd

import models.enumeration.EventType
import models.enumeration.EventType.{Event10, Event13}
//import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._

object EventSummary {

  //  implicit val rds: Reads[Seq[EventType]] = (
  //    (JsPath \ "periodStartDate").read[String] and
  //      (JsPath \ "periodEndDate").read[String] and
  //      (JsPath \ "tpssReportPresent").readNullable[String].flatMap {
  //        case Some("Yes") => Reads(_ => JsSuccess(true))
  //        case _ => Reads(_ => JsSuccess(false))
  //      }
  //    ) (
  //    (endDate, tpssReport, versionDetails) =>
  //     Nil
  //  )

  private val readsIsEventTypePresentFromSeq: Reads[Boolean] = {
    Reads {
      case JsArray(eventDetails) =>
        JsSuccess(
          eventDetails.exists {
            item => item \ "recordVersion" match {
                case JsDefined(JsString("001")) => true
                case _ => false
              }
          }
        )
      case _ =>
        JsError("error.invalid")
    }
  }

  implicit val rds: Reads[Seq[EventType]] = {
    val readsBooleanEvent10 = (JsPath \ "eventDetails" \ "event10").read[Boolean](readsIsEventTypePresentFromSeq)
    val readsBooleanEvent13 = (JsPath \ "eventDetails" \ "event13").read[Boolean](readsIsEventTypePresentFromSeq)
    for {
      event10 <- readsBooleanEvent10
      event13 <- readsBooleanEvent13
    } yield {
      booleanToValue(event10, Event10) ++
      booleanToValue(event13, Event13)
    }
  }

  private def booleanToValue[A](b: Boolean, v: A) = {
    if (b) Seq(v) else Nil
  }
}

