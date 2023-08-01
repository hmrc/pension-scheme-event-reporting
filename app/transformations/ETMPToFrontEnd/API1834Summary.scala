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
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._


object API1834Summary {

  private final val FieldNameRecordVersion = "recordVersion"

  private val readsRecordVersion: Reads[Int] = {
    Reads {
      case JsString(v) => JsSuccess(v.toInt)
      case JsNumber(v) => JsSuccess(v.toInt)
      case s => JsError(s"Invalid json $s")
    }
  }

  private val readsRecordVersionForEvent1: Reads[Option[Int]] = (
    (JsPath \ FieldNameRecordVersion).readNullable[Int](readsRecordVersion) and
      (JsPath \ "numberOfMembers").readNullable[Int]
    )(
    (recordVersion, numberMembers) =>
      (recordVersion, numberMembers) match {
        case (v, Some(n)) if n > 0 => v
        case _ => None
      }
  )

  private def createRow(event: Option[Int], eventType: String): JsObject = {
    event.fold(Json.obj())(version =>
      Json.obj(
        "eventType" -> eventType,
        FieldNameRecordVersion -> version
      )
    )
  }

  private def memberReads: Reads[Seq[JsObject]] = {
    val seqEventTypes = EventType.getMemberEvents

    def combineReads(list: Seq[Reads[Option[Int]]]): Reads[Seq[Option[Int]]] = list
      .foldLeft(Reads.pure(Seq.empty[Option[Int]])) { (acc, reads) =>
        for {
          acc <- acc
          reads <- reads
        } yield {
          acc :+ reads
        }
      }

    for {
      recordVersions <- combineReads(seqEventTypes.map(readsMemberRecordVersion))
      numberOfMembers <- combineReads(seqEventTypes.map(readsNumberOfMembers))
    } yield {
      seqEventTypes.zip(recordVersions).zip(numberOfMembers).map { case ((eventType, recordVersion), numberOfMembers) =>
        numberOfMembers match {
          case Some(0) => Json.obj()
          case _ => createRow(recordVersion, eventType.toString)
        }
      }
    }
  }

  private def readsMemberRecordVersion(eventType: EventType): Reads[Option[Int]] =
    (JsPath \ "memberEventsSummary" \ ("event" + eventType) \ FieldNameRecordVersion).readNullable[Int](readsRecordVersion)

  private def readsNumberOfMembers(eventType: EventType): Reads[Option[Int]] =
    (JsPath \ "memberEventsSummary" \ ("event" + eventType) \ "numberOfMembers")
      .readNullable[Int](readsRecordVersion)


  implicit val rdsFor1834: Reads[JsArray] = {
    val readsSeqInt = (
      (JsPath \ "event1ChargeDetails").read[Option[Int]](readsRecordVersionForEvent1) and
        memberReads and
        (JsPath \ "eventDetails" \ "event10" \ 0 \ FieldNameRecordVersion).readNullable[Int](readsRecordVersion) and
        (JsPath \ "eventDetails" \ "event11" \ FieldNameRecordVersion).readNullable[Int](readsRecordVersion) and
        (JsPath \ "eventDetails" \ "event12" \ FieldNameRecordVersion).readNullable[Int](readsRecordVersion) and
        (JsPath \ "eventDetails" \ "event13" \ 0 \ FieldNameRecordVersion).readNullable[Int](readsRecordVersion) and
        (JsPath \ "eventDetails" \ "event14" \ FieldNameRecordVersion).readNullable[Int](readsRecordVersion) and
        (JsPath \ "eventDetails" \ "event18" \ FieldNameRecordVersion).readNullable[Int](readsRecordVersion) and
        (JsPath \ "eventDetails" \ "event19" \ 0 \ FieldNameRecordVersion).readNullable[Int](readsRecordVersion) and
        (JsPath \ "eventDetails" \ "event20" \ 0 \ FieldNameRecordVersion).readNullable[Int](readsRecordVersion) and
        (JsPath \ "eventDetails" \ "eventWindUp" \ FieldNameRecordVersion).readNullable[Int](readsRecordVersion)
      )(
      (event1, memberReads, event10, event11, event12, event13, event14, event18, event19, event20, eventWindup) => {
        val result = Seq(createRow(event1, "1")) ++ memberReads ++ Seq(
          createRow(event10, "10"),
          createRow(event11, "11"),
          createRow(event12, "12"),
          createRow(event13, "13"),
          createRow(event14, "14"),
          createRow(event18, "18"),
          createRow(event19, "19"),
          createRow(event20, "20")
        ) ++ Seq(
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
      (JsPath \ "er20aDetails" \ "reportVersionNumber").readNullable[Int](readsRecordVersion).map {
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

