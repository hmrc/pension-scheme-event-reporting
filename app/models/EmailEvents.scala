/*
 * Copyright 2024 HM Revenue & Customs
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

package models

import models.enumeration.{Enumerable, WithName}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

sealed trait Event

object Event extends Enumerable.Implicits {

  override def toString: String = super.toString.toLowerCase

  implicit val enumerable: Enumerable[Event] = Enumerable(
    Seq(Sent, Delivered, PermanentBounce, Opened, Complained).map(v => v.toString -> v)*
  )
}

case object Sent extends WithName("Sent") with Event

case object Delivered extends WithName("Delivered") with Event

case object PermanentBounce extends WithName("PermanentBounce") with Event

case object Opened extends WithName("Opened") with Event

case object Complained extends WithName("Complained") with Event

case class EmailEvent(event: Event, detected: ZonedDateTime)

object EmailEvent {

  private val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  implicit val dateTimeWrite: Writes[ZonedDateTime] = new Writes[ZonedDateTime] {
    def writes(dateTime: ZonedDateTime): JsValue = JsString(dateTimeFormatter.format(dateTime))
  }

  implicit val read: Reads[EmailEvent] = {
    ((JsPath \ "event").read[Event] and ((JsPath \ "detected").read[String] map ZonedDateTime.parse))(EmailEvent.apply)
  }

  implicit val write: Writes[EmailEvent] = (
    (JsPath \ "event").write[Event] and (JsPath \ "detected").write[ZonedDateTime]
    )(emailEvent => (emailEvent.event, emailEvent.detected))

}

case class EmailEvents(events: Seq[EmailEvent])

object EmailEvents {
  implicit val format: OFormat[EmailEvents] = Json.format[EmailEvents]
}

