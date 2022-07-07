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

package models.enumeration

sealed trait EventType

object EventType extends Enumerable.Implicits {

  case object WindUp extends WithName("Event0") with EventType

  case object Event1 extends WithName("Event1") with EventType

  case object Event2 extends WithName("Event2") with EventType

  case object Event3 extends WithName("Event3") with EventType

  case object Event4 extends WithName("Event4") with EventType

  case object Event5 extends WithName("Event5") with EventType

  case object Event6 extends WithName("Event6") with EventType

  case object Event7 extends WithName("Event7") with EventType

  case object Event8 extends WithName("Event8") with EventType

  case object Event8A extends WithName("Event8A") with EventType

  case object Event10 extends WithName("Event10") with EventType

  case object Event11 extends WithName("Event11") with EventType

  case object Event12 extends WithName("Event12") with EventType

  case object Event13 extends WithName("Event13") with EventType

  case object Event14 extends WithName("Event14") with EventType

  case object Event18 extends WithName("Event18") with EventType

  case object Event19 extends WithName("Event19") with EventType

  case object Event20 extends WithName("Event20") with EventType

  case object Event20A extends WithName("Event20A") with EventType

  case object Event22 extends WithName("Event22") with EventType

  case object Event23 extends WithName("Event23") with EventType

  case object Event24 extends WithName("Event24") with EventType

  val values: List[EventType] = List(WindUp, Event1, Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A,
    Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, Event20A, Event22, Event23, Event24)

  val api1826Events: List[EventType] = List(Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, WindUp)
  val api1827Events: List[EventType] = List(Event1)
  val api1829Events: List[EventType] = List(Event20A)
  val api1830Events: List[EventType] = List(Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A, Event22, Event23, Event24)
  val api1832Events: List[EventType] = List(Event3, Event4, Event5, Event22, Event23, Event24, Event2, Event6, Event7, Event8, Event8A)

  def getEventType(s: String): Option[EventType] = values.find(_.toString == s)

  def apiTypeByEventTypePOST(eventType: EventType): Option[ApiType] = {
    if (api1826Events.contains(eventType)) {
      Some(ApiType.Api1826)
    } else if (api1827Events.contains(eventType)) {
      Some(ApiType.Api1827)
    } else if (api1829Events.contains(eventType)) {
      Some(ApiType.Api1829)
    } else if (api1830Events.contains(eventType)) {
      Some(ApiType.Api1830)
    } else {
      None
    }
  }

  def apiTypeByEventTypeGET(eventType: EventType): Option[ApiType] = {
    if (api1832Events.contains(eventType)) {
      Some(ApiType.Api1832)
    } else {
      None
    }
  }
}
