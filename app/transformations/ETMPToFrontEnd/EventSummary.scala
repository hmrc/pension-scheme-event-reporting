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

  private val readsRecordVersionNo: Reads[Boolean] = {
    Reads {
      case JsArray(eventDetails) =>
        JsSuccess(
          eventDetails.exists {
            item =>
              (item \ "recordVersion") match {
                case JsDefined(ddd) => ddd.toString == "001"
                case _ => false
              }
          }
        )
      case _ =>
        JsError("error.invalid")
    }
  }

  implicit val rds: Reads[Seq[EventType]] = {
    val tt = (JsPath \ "eventDetails" \ "event10").read[Boolean](readsRecordVersionNo)
    val readsOptionEventTypeEvent10: Reads[Seq[EventType]] = tt.flatMap { uu =>
      if (uu) {
        Some(EventType.Event10)
      } else {
        None
      }
      ///
      readsOptionEventTypeEvent10
    }
    readsOptionEventTypeEvent10.map { kkk =>
      kkk.toSeq
    }
  }


}

