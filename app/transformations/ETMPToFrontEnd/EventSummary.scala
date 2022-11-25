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
import models.enumeration.EventType._
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._

object EventSummary {

  private val FieldNameRecordVersion = "recordVersion"

  private val readsIsEventTypePresentFromSeq: Reads[Boolean] = {
    Reads {
      case JsArray(eventDetails) =>
        JsSuccess(
          eventDetails.exists {
            item =>
              item \ FieldNameRecordVersion match {
                case JsDefined(JsString("001")) => true
                case _ => false
              }
          }
        )
      case e =>
        JsError(s"Invalid json $e")
    }
  }

  private val readsIsEventTypePresent: Reads[Boolean] = {
    Reads {
      case JsString("001") =>
        JsSuccess(true)
      case _ =>
        JsSuccess(false)
    }
  }
  implicit val rds: Reads[JsArray] = {
    val sortEventTypes: (String, String) => Boolean = (a, b) =>
      (a, b) match {
        case ("0", _) => false
        case (_, "0") => true
        case (a, b) if a < b => true
        case _ => false
      }

    def booleanToValue(b: Option[Boolean], v: EventType): Seq[String] = if (b.getOrElse(false)) Seq(v.toString) else Nil
    (
      (JsPath \ "eventDetails" \ "event2").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event3").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event4").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event5").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event6").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event7").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event8").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event8A").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event10").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event11" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) and
      (JsPath \ "eventDetails" \ "event12" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) and
      (JsPath \ "eventDetails" \ "event13").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event14" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) and
      (JsPath \ "eventDetails" \ "event18" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) and
      (JsPath \ "eventDetails" \ "event19").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event20").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event22").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event23").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "event24").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
      (JsPath \ "eventDetails" \ "eventWindUp" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent)
      ) (
      (event2, event3, event4, event5, event6, event7, event8, event8A, event10, event11,
       event12, event13, event14, event18, event19, event20, event22, event23, event24, eventWindUp) => {
        val seqJsString =
          booleanToValue(event2, Event2) ++
          booleanToValue(event3, Event3) ++
          booleanToValue(event4, Event4) ++
          booleanToValue(event5, Event5) ++
          booleanToValue(event6, Event6) ++
          booleanToValue(event7, Event7) ++
          booleanToValue(event8, Event8) ++
          booleanToValue(event8A, Event8A) ++
          booleanToValue(event10, Event10) ++
          booleanToValue(event11, Event11) ++
          booleanToValue(event12, Event12) ++
          booleanToValue(event13, Event13) ++
          booleanToValue(event14, Event14) ++
          booleanToValue(event18, Event18) ++
          booleanToValue(event19, Event19) ++
          booleanToValue(event20, Event20) ++
          booleanToValue(event22, Event22) ++
          booleanToValue(event23, Event23) ++
          booleanToValue(event24, Event24) ++
          booleanToValue(eventWindUp, WindUp)
        JsArray(seqJsString.sortWith(sortEventTypes).map(JsString))
      }
    )
  }
}

