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

  /**
   * Used for getting summaries for all of the events except for Event1 and Event22A -Pavel Vjalicin
   */
  implicit val rdsFor1834: Reads[JsArray] = {

    val readsSeq:Seq[(Reads[Option[Boolean]], EventType)] = Seq(
      (JsPath \ "event1ChargeDetails" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event1,
      (JsPath \ "memberEventsSummary" \ "event2" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event2,
        (JsPath \ "memberEventsSummary" \ "event3" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event3,
        (JsPath \ "memberEventsSummary" \ "event4" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event4,
        (JsPath \ "memberEventsSummary" \ "event5" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event5,
        (JsPath \ "memberEventsSummary" \ "event6" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event6,
        (JsPath \ "memberEventsSummary" \ "event7" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event7,
        (JsPath \ "memberEventsSummary" \ "event8" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event8,
        (JsPath \ "memberEventsSummary" \ "event8A" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event8A,
      (JsPath \ "eventDetails" \ "event10").readNullable[Boolean](readsIsEventTypePresentFromSeq) -> Event10,
        (JsPath \ "eventDetails" \ "event11" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event11,
        (JsPath \ "eventDetails" \ "event12" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event12,
        (JsPath \ "eventDetails" \ "event13").readNullable[Boolean](readsIsEventTypePresentFromSeq) -> Event13,
        (JsPath \ "eventDetails" \ "event14" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event14,
        (JsPath \ "eventDetails" \ "event18" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event18,
        (JsPath \ "eventDetails" \ "event19").readNullable[Boolean](readsIsEventTypePresentFromSeq) -> Event19,
        (JsPath \ "eventDetails" \ "event20").readNullable[Boolean](readsIsEventTypePresentFromSeq) -> Event20,
        (JsPath \ "memberEventsSummary" \ "event22" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event22,
        (JsPath \ "memberEventsSummary" \ "event23" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event23,
        (JsPath \ "memberEventsSummary" \ "event24" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> Event24,
        (JsPath \ "eventDetails" \ "eventWindUp" \ "recordVersion").readNullable[Boolean](readsIsEventTypePresent) -> WindUp
      )

    def modifyReads(reads:Reads[Option[Boolean]], event: EventType) =  reads.map(x => JsArray(booleanToValue(x, event).map(JsString)))

    val head = readsSeq.head

    readsSeq.tail.foldLeft( modifyReads(head._1, head._2) ) { case (acc, (reads, event)) =>
      (acc and  modifyReads(reads, event)) ((r1, r2) => r1 ++ r2)
    }
  }

  /**
   * Used for getting summary for Event20A -Sharad Jamdade
   */
  implicit val rdsFor1831: Reads[JsArray] = {
    val readsSeq: Seq[(Reads[Option[Boolean]], EventType)] = Seq(
      (JsPath \ "er20aDetails" \ "reportVersionNumber").readNullable[Boolean](readsIsEventTypePresent) -> Event20A
    )

    def modifyReads(reads: Reads[Option[Boolean]], event: EventType) = reads.map(x => JsArray(booleanToValue(x, event).map(JsString)))

    val head = readsSeq.head
    readsSeq.tail.foldLeft(modifyReads(head._1, head._2)) { case (acc, (reads, event)) =>
      (acc and modifyReads(reads, event))((r1, r2) => r1 ++ r2)
    }
  }
  private def booleanToValue(b: Option[Boolean], v: EventType): Seq[String] = if (b.getOrElse(false)) Seq(v.toString) else Nil

}

