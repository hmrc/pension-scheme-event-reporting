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
import play.api.libs.json._

import scala.language.implicitConversions


object EventSummary {

  private val fieldNameRecordVersion = "recordVersion"
  private val memberEvents: List[EventType] = List(Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A, Event22, Event23, Event24)

  private val readsIsEventTypePresentFromSeq: Reads[Boolean] = {
    Reads {
      case JsArray(eventDetails) =>
        JsSuccess(
          eventDetails.exists {
            item =>
              item \ fieldNameRecordVersion match {
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
      case JsString(_) =>
        JsSuccess(true)
      case _ =>
        JsSuccess(false)
    }
  }

  implicit def rdsEventTypeNodeOnly(eventType: EventType): Reads[JsArray] = {
    (JsPath \ "eventReportDetails" \ "eventType").readNullable[String] map {
      case Some(_) => JsArray(Seq(JsString(eventType.toString)))
      case _ => JsArray(Seq.empty)
    }
  }

  /**
   * Used for getting summaries for all of the events except for Event1 and Event22A -Pavel Vjalicin
   */
  implicit val rdsFor1834: Reads[JsArray] = {

    val memberEventsJsArray = (JsPath \ "memberEventsSummary")
      .readNullable[JsArray](Reads {
        case JsObject(obj) =>
          val list = memberEvents.flatMap(event =>
            obj.get("event" + event.toString).map(_ => event.toString)
          ).map(JsString)
          JsSuccess(JsArray(list))
        case _ => JsSuccess(JsArray())
      }).map(_.getOrElse(JsArray()))

    val nonMemberEventsJsArray = (
      (JsPath \ "eventDetails" \ "event10").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
        (JsPath \ "eventDetails" \ "event11" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event12" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event13").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
        (JsPath \ "eventDetails" \ "event14" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event18" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event19").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
        (JsPath \ "eventDetails" \ "event20").readNullable[Boolean](readsIsEventTypePresentFromSeq) and
        (JsPath \ "eventDetails" \ "eventWindUp" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent)
      ) (
      ( event10, event11, event12, event13, event14, event18, event19, event20, eventWindUp) => {
        val seqString = {
          booleanToValue(event10, Event10) ++
            booleanToValue(event11, Event11) ++
            booleanToValue(event12, Event12) ++
            booleanToValue(event13, Event13) ++
            booleanToValue(event14, Event14) ++
            booleanToValue(event18, Event18) ++
            booleanToValue(event19, Event19) ++
            booleanToValue(event20, Event20) ++
            booleanToValue(eventWindUp, WindUp)
        }
        JsArray(seqString.map(JsString))
      }
    )
    (memberEventsJsArray and nonMemberEventsJsArray)( (a1, a2) => a1 ++ a2 )
  }

  private def booleanToValue(b: Option[Boolean], v: EventType): Seq[String] = if (b.getOrElse(false)) Seq(v.toString) else Nil

}

