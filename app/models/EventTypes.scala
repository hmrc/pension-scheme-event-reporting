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

package models

object EventTypes extends Enumeration {

  sealed case class TypeValue(eventName: String, eventValue: String) extends Val(eventName)

   private val windUp = TypeValue("0", "0")
   private val event1 = TypeValue("1", "1")
   private val event2 = TypeValue("2", "2")
   private val event3 = TypeValue("3", "3")
   private val event4 = TypeValue("4", "4")
   private val event5 = TypeValue("5", "5")
   private val event6 = TypeValue("6", "6")
   private val event7 = TypeValue("7", "7")
   private val event8 = TypeValue("8", "8")
   private val event8A = TypeValue("8A", "8A")
   private val event10 = TypeValue("10", "10")
   private val event11 = TypeValue("11", "11")
   private val event12 = TypeValue("12", "12")
   private val event13 = TypeValue("13", "13")
   private val event14 = TypeValue("14", "14")
   private val event18 = TypeValue("18", "18")
   private val event19 = TypeValue("19", "19")
   private val event20 = TypeValue("20", "20")
   private val event20A = TypeValue("20A", "20A")
   private val event22 = TypeValue("22", "22")
   private val event23 = TypeValue("23", "23")
   private val event24 = TypeValue("24", "24")

 val api1826Events = List(event10, event11, event12, event13, event14, event18, event19, event20, windUp)
 val api1827Events = List(event1)
 val api1829Events = List(event20A)
 val api1832Events = List(event2, event3, event4, event5, event6, event7, event8, event8A, event22, event23, event24)

  def valueWithName(name: String): String = {
    super.withName(name).asInstanceOf[TypeValue].eventValue
  }

  def nameWithValue(value: String): Option[String] = {
    Seq(windUp, event1, event2, event3, event4, event5, event6, event7, event8, event8A, event10, event11,
      event12, event13, event14, event18, event19, event20, event20A, event22, event23, event24)
      .find(_.eventValue == value).map(_.eventName)
  }
}
