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
    def booleanToValue(b: Option[Boolean], v: EventType): Seq[JsString] = if (b.getOrElse(false)) Seq(JsString(v.toString)) else Nil
    val readsBooleanEvent10 = (JsPath \ "eventDetails" \ "event10").readNullable[Boolean](readsIsEventTypePresentFromSeq)
    val readsBooleanEvent11 = (JsPath \ "eventDetails" \ "event11" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent)
    val readsBooleanEvent12 = (JsPath \ "eventDetails" \ "event12" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent)
    val readsBooleanEvent13 = (JsPath \ "eventDetails" \ "event13").readNullable[Boolean](readsIsEventTypePresentFromSeq)
    val readsBooleanEvent14 = (JsPath \ "eventDetails" \ "event14" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent)
    val readsBooleanEvent18 = (JsPath \ "eventDetails" \ "event18" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent)
    val readsBooleanEvent19 = (JsPath \ "eventDetails" \ "event19").readNullable[Boolean](readsIsEventTypePresentFromSeq)
    val readsBooleanEvent20 = (JsPath \ "eventDetails" \ "event20").readNullable[Boolean](readsIsEventTypePresentFromSeq)
    val readsBooleanEventWindUp = (JsPath \ "eventDetails" \ "eventWindUp" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent)
    for {
      event10 <- readsBooleanEvent10
      event11 <- readsBooleanEvent11
      event12 <- readsBooleanEvent12
      event13 <- readsBooleanEvent13
      event14 <- readsBooleanEvent14
      event18 <- readsBooleanEvent18
      event19 <- readsBooleanEvent19
      event20 <- readsBooleanEvent20
      eventWindUp <- readsBooleanEventWindUp
    } yield {
      val seqJsString = booleanToValue(event10, Event10) ++
        booleanToValue(event11, Event11) ++
        booleanToValue(event12, Event12) ++
        booleanToValue(event13, Event13) ++
        booleanToValue(event14, Event14) ++
        booleanToValue(event18, Event18) ++
        booleanToValue(event19, Event19) ++
        booleanToValue(event20, Event20) ++
        booleanToValue(eventWindUp, WindUp)
      JsArray(seqJsString.sortWith((a,b) => a.value < b.value))
    }
  }

}

