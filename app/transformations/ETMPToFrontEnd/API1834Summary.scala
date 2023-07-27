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
      case JsNumber(v) => JsSuccess(v.toInt)
      case s => JsError(s"Invalid json $s")
    }
  }
  private def createRow(event: Option[Int], eventType: String, numberOfMembers: Option[Int] = None): JsObject = {
    event.fold(Json.obj())(version =>
      Json.obj(
        "eventType" -> eventType,
        "recordVersion" -> version
      ) ++ numberOfMembers.map { x => Json.obj("numberOfMembers" -> JsNumber(x))}.getOrElse(Json.obj())
    )
  }

  private def memberReads(list:List[EventType]) = {
    def combineReads(list: List[Reads[Option[Int]]]) = list
      .foldLeft(Reads.pure(Seq.empty[Option[Int]])) { (acc, reads) =>
        for {
          acc <- acc
          reads <- reads
        } yield {
          acc :+ reads
        }
      }

    for {
      recordVersions <- combineReads(list.map(readsMemberRecordVersion))
      numberOfMembers <- combineReads(list.map(readsNumberOfMembers))
    } yield {
      list.zip(recordVersions).zip(numberOfMembers).map { case ((eventType, recordVersion), numberOfMembers) =>
        createRow(recordVersion, eventType.toString, numberOfMembers)
      }
    }

  }
  private def readsMemberRecordVersion(eventType: EventType) =
    (JsPath \ "memberEventsSummary" \ ("event" + eventType) \ "recordVersion").readNullable[Int](readsIsEventTypePresent)

  private def readsNumberOfMembers(eventType: EventType) =
    (JsPath \ "memberEventsSummary" \ ("event" + eventType) \ "numberOfMembers").readNullable[Int](readsIsEventTypePresent)

  implicit val rdsFor1834: Reads[JsArray] = {
    val readsSeqInt = (
      (JsPath \ "event1ChargeDetails" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        memberReads (List(Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A)) and
        (JsPath \ "eventDetails" \ "event10" \ 0 \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event11" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event12" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event13" \ 0 \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event14" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event18" \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event19" \ 0 \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        (JsPath \ "eventDetails" \ "event20" \ 0 \ "recordVersion").readNullable[Int](readsIsEventTypePresent) and
        memberReads(List(Event22, Event23)) and
        (JsPath \ "eventDetails" \ "eventWindUp" \ "recordVersion").readNullable[Int](readsIsEventTypePresent)
      )(
      (event1, memberReads, event10, event11, event12, event13, event14, event18, event19, event20, event22and23, eventWindup) => {
        val result = Seq(
          createRow(event1, "1"),
        ) ++ memberReads ++ Seq(
          createRow(event10, "10"),
          createRow(event11, "11"),
          createRow(event12, "12"),
          createRow(event13, "13"),
          createRow(event14, "14"),
          createRow(event18, "18"),
          createRow(event19, "19"),
          createRow(event20, "20")
        ) ++ event22and23 ++ Seq(
          createRow(eventWindup, "WindUp")
        )

        result.filter(_.fields.nonEmpty)
      }
    )
    readsSeqInt.map { s =>
      JsArray(s)
    }
  }


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

