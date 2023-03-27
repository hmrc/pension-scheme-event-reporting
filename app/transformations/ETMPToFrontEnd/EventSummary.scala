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

  implicit val rdsFor1834: Reads[JsArray] = {

    (
      (JsPath \ "memberEventsSummary" \ "event6").readNullable[Boolean](readsIsEventTypePresent) and
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
      (event6, event10, event11, event12, event13, event14, event18, event19, event20, eventWindUp) => {
        val seqString = {
          booleanToValue(event6, Event6) ++
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
  }

  private def booleanToValue(b: Option[Boolean], v: EventType): Seq[String] = if (b.getOrElse(false)) Seq(v.toString) else Nil

}

