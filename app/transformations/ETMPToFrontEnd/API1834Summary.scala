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
          createRow(event1, "event1"),
          createRow(event2, "event2"),
          createRow(event3, "event3"),
          createRow(event4, "event4"),
          createRow(event5, "event5"),
          createRow(event6, "event6"),
          createRow(event7, "event7"),
          createRow(event8, "event8"),
          createRow(event8A, "event8A"),
          createRow(event10, "event10"),
          createRow(event11, "event11"),
          createRow(event12, "event12"),
          createRow(event13, "event13"),
          createRow(event14, "event14"),
          createRow(event18, "event18"),
          createRow(event19, "event19"),
          createRow(event20, "event20"),
          createRow(event22, "event22"),
          createRow(event23, "event23"),
          createRow(eventWindup, "eventWindUp")

        ).filter(_.fields.nonEmpty)
      }
    )
    readsSeqInt.map { s =>
      JsArray(s)
    }
  }


  //    val readsSeq = Seq(
  //      Event1 -> (JsPath \ "event1ChargeDetails" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent),
  //      (JsPath \ "memberEventsSummary" \ "event2" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event2,
  //        (JsPath \ "memberEventsSummary" \ "event3" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event3,
  //        (JsPath \ "memberEventsSummary" \ "event4" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event4,
  //        (JsPath \ "memberEventsSummary" \ "event5" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event5,
  //        (JsPath \ "memberEventsSummary" \ "event6" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event6,
  //        (JsPath \ "memberEventsSummary" \ "event7" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event7,
  //        (JsPath \ "memberEventsSummary" \ "event8" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event8,
  //        (JsPath \ "memberEventsSummary" \ "event8A" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event8A,
  //      (JsPath \ "eventDetails" \ "event10").readNullable[Option[String]](readsIsEventTypePresentFromSeq) -> Event10,
  //        (JsPath \ "eventDetails" \ "event11" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event11,
  //        (JsPath \ "eventDetails" \ "event12" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event12,
  //        (JsPath \ "eventDetails" \ "event13").readNullable[Option[String]](readsIsEventTypePresentFromSeq) -> Event13,
  //        (JsPath \ "eventDetails" \ "event14" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event14,
  //        (JsPath \ "eventDetails" \ "event18" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event18,
  //        (JsPath \ "eventDetails" \ "event19").readNullable[Option[String]](readsIsEventTypePresentFromSeq) -> Event19,
  //        (JsPath \ "eventDetails" \ "event20").readNullable[Option[String]](readsIsEventTypePresentFromSeq) -> Event20,
  //        (JsPath \ "memberEventsSummary" \ "event22" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event22,
  //        (JsPath \ "memberEventsSummary" \ "event23" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> Event23,
  //        (JsPath \ "eventDetails" \ "eventWindUp" \ "recordVersion").readNullable[Option[String]](readsIsEventTypePresent) -> WindUp
  //      )
  //
  //    def modifyReads(reads:Reads[Option[Boolean]], event: EventType) =  reads.map(x => JsArray(booleanToValue(x, event).map(JsString)))
  //
  //    val head = readsSeq.head
  //
  //    readsSeq.tail.foldLeft( modifyReads(head._1, head._2) ) { case (acc, (reads, event)) =>
  //      (acc and  modifyReads(reads, event)) ((r1, r2) => r1 ++ r2)
  //    }


  /**
   * Used for getting summary for Event20A -Sharad Jamdade
   */
  implicit val rdsFor1831: Reads[JsArray] = {
    Reads.pure(JsArray.empty)
    //  val readsSeq: Seq[(Reads[Option[Boolean]], EventType)] = Seq(
    //    (JsPath \ "er20aDetails" \ "reportVersionNumber").readNullable[Boolean](readsIsEventTypePresent) -> Event20A
    //  )
    //
    //  def modifyReads(reads: Reads[Option[Boolean]], event: EventType) = reads.map(x => JsArray(booleanToValue(x, event).map(JsString)))
    //
    //  val head = readsSeq.head
    //  readsSeq.tail.foldLeft(modifyReads(head._1, head._2)) { case (acc, (reads, event)) =>
    //    (acc and modifyReads(reads, event))((r1, r2) => r1 ++ r2)
    //  }
  }

  private def booleanToValue(b: Option[Boolean], v: EventType): Seq[String] = if (b.getOrElse(false)) Seq(v.toString) else Nil

}

