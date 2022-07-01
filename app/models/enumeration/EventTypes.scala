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

sealed trait EventTypes

object EventTypes extends Enumerable.Implicits {

  case object WindUp extends WithName("0") with EventTypes

  case object Event1 extends WithName("1") with EventTypes

  case object Event2 extends WithName("2") with EventTypes

  case object Event3 extends WithName("3") with EventTypes

  case object Event4 extends WithName("4") with EventTypes

  case object Event5 extends WithName("5") with EventTypes

  case object Event6 extends WithName("6") with EventTypes

  case object Event7 extends WithName("7") with EventTypes

  case object Event8 extends WithName("8") with EventTypes

  case object Event8A extends WithName("8A") with EventTypes

  case object Event10 extends WithName("10") with EventTypes

  case object Event11 extends WithName("11") with EventTypes

  case object Event12 extends WithName("12") with EventTypes

  case object Event13 extends WithName("13") with EventTypes

  case object Event14 extends WithName("14") with EventTypes

  case object Event18 extends WithName("18") with EventTypes

  case object Event19 extends WithName("19") with EventTypes

  case object Event20 extends WithName("20") with EventTypes

  case object Event20A extends WithName("20A") with EventTypes

  case object Event22 extends WithName("22") with EventTypes

  case object Event23 extends WithName("23") with EventTypes

  case object Event24 extends WithName("24") with EventTypes

  val values = List(WindUp, Event1, Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A,
    Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, Event20A, Event22, Event23, Event24)

  val api1826Events = List(Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, WindUp)
  val api1827Events = List(Event1)
  val api1829Events = List(Event20A)
  val api1832Events = List(Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A, Event22, Event23, Event24)


  def getEventTypes(s: String): Option[EventTypes] = values.find(_.toString == s)

  def getApiTypesByEventType(eventType: EventTypes): Option[ApiTypes] = {

    if (api1826Events.contains(eventType)) {
      Some(ApiTypes.Api1826)
    } else if (api1827Events.contains(eventType)) {
      Some(ApiTypes.Api1827)
    } else if (api1829Events.contains(eventType)) {
      Some(ApiTypes.Api1829)
    } else if (api1832Events.contains(eventType)) {
      Some(ApiTypes.Api1832)
    }else{
      None
    }
  }
}
