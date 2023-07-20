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

import models.enumeration.EventType
import models.enumeration.EventType._
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{JsArray, _}


object API1834Summary {

  private val fieldNameRecordVersion = "recordVersion"

  private val readsIsEventTypePresent: Reads[Int] = {
    Reads {
      case JsString(v) => JsSuccess(v.toInt)
      case s => JsError(s"Invalid json $s")
    }
  }
  private def createRow(event: Option[Int], eventType: String): JsObject = {
    event.fold(Json.obj())(version =>
      Json.obj(
        "eventType" -> eventType,
        "recordVersion" -> version
      )
    )
  }

  private val readsIsEventTypePresentAPI1831: Reads[Boolean] = {
    Reads {
      case JsString(_) =>
        JsSuccess(true)
      case _ =>
        JsSuccess(false)
    }
  }

  /**
   * Used for getting summaries for all of the events except for Event1 and Event22A -Pavel Vjalicin
   */
  implicit val rdsFor1834: Reads[JsArray] = {

    val readsSeqInt = (
      (JsPath \ "event1ChargeDetails" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "memberEventsSummary" \ "event2" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "memberEventsSummary" \ "event3" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "memberEventsSummary" \ "event4" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "memberEventsSummary" \ "event5" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "memberEventsSummary" \ "event6" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "memberEventsSummary" \ "event7" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "memberEventsSummary" \ "event8" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "memberEventsSummary" \ "event8A" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event10" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event11" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event12" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event13" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event14" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event18" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event19" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event20" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "memberEventsSummary" \ "event22" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "memberEventsSummary" \ "event23" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "eventWindUp" \ "recordVersion").readNullable[Int](readsIsEventTypePresent)
      )(
      (event1, event2, event3, event4, event5, event6, event7, event8, event8A, event10, event11, event12, event13, event14, event18, event19, event20, event22, event23, eventWindup) => {
        Seq(
          createRow(event1, "1"),
          createRow(event2, "2"),
          createRow(event3, "3"),
          createRow(event4, "4"),
          createRow(event5, "5"),
          createRow(event6, "6"),
          createRow(event7, "7"),
          createRow(event8, "8"),
          createRow(event8A, "8A"),
          createRow(event10, "10"),
          createRow(event11, "11"),
          createRow(event12, "12"),
          createRow(event13, "13"),
          createRow(event14, "14"),
          createRow(event18, "18"),
          createRow(event19, "19"),
          createRow(event20, "20"),
          createRow(event22, "22"),
          createRow(event23, "23"),
          createRow(eventWindup, "WindUp")

        ).filter(_.fields.nonEmpty)
      }
    )
    readsSeqInt.map { s =>
      JsArray(s)
    }
  }



  /**
   * Used for getting summary for Event20A -Sharad Jamdade
   */
  implicit val rdsFor1831: Reads[JsArray] = {
    val readsSeqInt =
      (JsPath \ "er20aDetails" \ "reportVersionNumber").readNullable[Int](readsIsEventTypePresent).map {
        event20a => {
          Seq(
            createRow(event20a, "20A")

          ).filter(_.fields.nonEmpty)
        }
      }

    readsSeqInt.map { s =>
      JsArray(s)
    }
  }
}

