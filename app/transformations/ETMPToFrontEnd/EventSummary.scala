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

  private val readsIsEventTypePresent: Reads[Boolean] = {
    Reads {
      case JsString("001") =>
        JsSuccess(true)
      case _ =>
        JsSuccess(false)
    }
  }

  implicit val rds: Reads[Seq[EventType]] = {
    val readsBooleanEvent10 = (JsPath \ "eventDetails" \ "event10").read[Boolean](readsIsEventTypePresentFromSeq)
    val readsBooleanEvent11 = (JsPath \ "eventDetails" \ "event11" \ "recordVersion").read[Boolean](readsIsEventTypePresent)
    val readsBooleanEvent12 = (JsPath \ "eventDetails" \ "event12" \ "recordVersion").read[Boolean](readsIsEventTypePresent)
    val readsBooleanEvent13 = (JsPath \ "eventDetails" \ "event13").read[Boolean](readsIsEventTypePresentFromSeq)
    val readsBooleanEvent14 = (JsPath \ "eventDetails" \ "event14" \ "recordVersion").read[Boolean](readsIsEventTypePresent)
    val readsBooleanEvent18 = (JsPath \ "eventDetails" \ "event18" \ "recordVersion").read[Boolean](readsIsEventTypePresent)
    val readsBooleanEvent19 = (JsPath \ "eventDetails" \ "event19").read[Boolean](readsIsEventTypePresentFromSeq)
    val readsBooleanEvent20 = (JsPath \ "eventDetails" \ "event20").read[Boolean](readsIsEventTypePresentFromSeq)
    val readsBooleanEventWindUp = (JsPath \ "eventDetails" \ "eventWindUp" \ "recordVersion").read[Boolean](readsIsEventTypePresent)
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
  }

  private def booleanToValue[A](b: Boolean, v: A) = {
    if (b) Seq(v) else Nil
  }
}

