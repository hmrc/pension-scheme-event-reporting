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
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsPath, JsString, JsSuccess, Reads, __}

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

  private val readsRecordVersionNo: Reads[String] = (
    (JsPath \ "recordVersion").read[String]
  )

//  ((__ \ 'chargeADetails \ 'amendedVersion).json.copyFrom((__ \ 'amendedVersion).json.pick) and

  implicit val rds: Reads[Seq[EventType]] = (
    (JsPath \ "eventDetails" \ "event10").read[String](readsRecordVersionNo)
  )
}

