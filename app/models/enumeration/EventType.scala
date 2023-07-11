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

package models.enumeration

import play.api.libs.json.{Format, JsError, JsResult, JsString, JsSuccess, JsValue}

sealed trait EventType {
  val order: Int
}

object EventType extends Enumerable.Implicits {

  case object WindUp extends WithName("WindUp") with EventType {
    override val order = 21
  }

  case object Event1 extends WithName("1") with EventType {
    override val order = 0
  }

  case object Event2 extends WithName("2") with EventType {
    override val order = 1
  }

  case object Event3 extends WithName("3") with EventType {
    override val order = 2
  }

  case object Event4 extends WithName("4") with EventType {
    override val order = 3
  }

  case object Event5 extends WithName("5") with EventType {
    override val order = 4
  }

  case object Event6 extends WithName("6") with EventType {
    override val order = 5
  }

  case object Event7 extends WithName("7") with EventType {
    override val order = 6
  }

  case object Event8 extends WithName("8") with EventType {
    override val order = 7
  }

  case object Event8A extends WithName("8A") with EventType {
    override val order = 8
  }

  case object Event10 extends WithName("10") with EventType {
    override val order = 9
  }

  case object Event11 extends WithName("11") with EventType {
    override val order = 10
  }

  case object Event12 extends WithName("12") with EventType {
    override val order = 11
  }

  case object Event13 extends WithName("13") with EventType {
    override val order = 12
  }

  case object Event14 extends WithName("14") with EventType {
    override val order = 13
  }

  case object Event18 extends WithName("18") with EventType {
    override val order = 14
  }

  case object Event19 extends WithName("19") with EventType {
    override val order = 15
  }

  case object Event20 extends WithName("20") with EventType {
    override val order = 16
  }

  case object Event20A extends WithName("20A") with EventType {
    override val order = 17
  }

  case object Event22 extends WithName("22") with EventType {
    override val order = 18
  }

  case object Event23 extends WithName("23") with EventType {
    override val order = 19
  }

  case object Event24 extends WithName("24") with EventType {
    override val order = 20
  }

  case object DummyForTest extends WithName("DummyForTest") with EventType {
    override val order = -1
  }

  case object EventTypeNone extends WithName("None") with EventType {
    override val order = -1
  }

  private val values: List[EventType] = List(WindUp, Event1, Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A,
    Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, Event20A, Event22, Event23, Event24, EventTypeNone)

  private val api1826Events: List[EventType] = List(Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, WindUp)
  private val api1827Events: List[EventType] = List(Event1)
  private val api1829Events: List[EventType] = List(Event20A)
  private val api1830Events: List[EventType] = List(Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A, Event22, Event23, Event24)
  private val api1832Events: List[EventType] = List(Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A, Event22, Event23, Event24)
  private val api1833Events: List[EventType] = List(Event1)
  private val api1831Events: List[EventType] = List(Event20A)
  private val api1834Events: List[EventType] = List(WindUp, Event10, Event18, Event13, Event20, Event11, Event12, Event14, Event19)

  def getEventType(s: String): Option[EventType] = values.find(_.toString == s)

  def getEventTypesForAPI(apiType: ApiType): Seq[EventType] =
    apiType match {
      case ApiType.Api1826 => api1826Events
      case _ => Nil
    }

  def postApiTypeByEventType(eventType: EventType): Option[ApiType] = {
    eventType match {
      case evType1826 if api1826Events.contains(evType1826) => Some(ApiType.Api1826)
      case evType1827 if api1827Events.contains(evType1827) => Some(ApiType.Api1827)
      case evType1829 if api1829Events.contains(evType1829) => Some(ApiType.Api1829)
      case evType1830 if api1830Events.contains(evType1830) => Some(ApiType.Api1830)
      case _ => None
    }
  }

  def getApiTypeByEventType(eventType: EventType): Option[ApiType] = {
    eventType match {
      case evType1832 if api1832Events.contains(evType1832) => Some(ApiType.Api1832)
      case evType1833 if api1833Events.contains(evType1833) => Some(ApiType.Api1833)
      case evType1831 if api1831Events.contains(evType1831) => Some(ApiType.Api1831)
      case evType1834 if api1834Events.contains(evType1834) => Some(ApiType.Api1834)
      case _ => None
    }
  }

  implicit val formats: Format[EventType] = new Format[EventType] {
    override def writes(o: EventType): JsValue = JsString(o.toString)

    override def reads(json: JsValue): JsResult[EventType] = {
      val jsonAsString = json.as[String]
      values.find(_.toString == jsonAsString) match {
        case Some(eventType) => JsSuccess(eventType)
        case None => JsError(s"Unknown event type: $jsonAsString")
      }
    }
  }

}
