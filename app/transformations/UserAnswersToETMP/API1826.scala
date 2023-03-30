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

package transformations.UserAnswersToETMP

import play.api.libs.json._
import transformations.Transformer

object API1826 extends Transformer {

  private sealed trait SchemeStructure
  private object SchemeStructure extends Enumeration {
    val single: Value = Value("A single trust under which all of the assets are held for the benefit of all members of the scheme")
    val group: Value = Value("A group life / death in service scheme")
    val corporate: Value = Value("A body corporate")
    val other: Value = Value("Other")
    implicit val format: Format[Value] = Json.formatEnum(this)
  }

  private implicit val event13DataFormat: OFormat[Event13Data] = Json.format[Event13Data]
  private case class Event13Data(
                                  recordVersion: Option[String],
                                  schemeStructure: SchemeStructure,
                                  schemeStructureOther: Option[String],
                                  dateOfChange:String
                                ) {
    private val recordVersionPattern = "[0-9]{3}".r

    private val dateOfChangePattern = "(((19|20)([2468][048]|[13579][26]|0[48])|2000)[-]02[-]29|((19|20)[0-9]{2}[-](0[469]|11)[-](0[1-9]|1[0-9]|2[0-9]|30)|(19|20)[0-9]{2}[-](0[13578]|1[02])[-](0[1-9]|[12][0-9]|3[01])|(19|20)[0-9]{2}[-]02[-](0[1-9]|1[0-9]|2[0-8])))".r

    private val schemeStructureOtherPattern = """[a-zA-Z0-9\s\\u00C0-\\u00FF!#$%&'‘’\\"“”«»()*+,.\/:;=?@\\\[\\\]£€¥\\u005C\\u2014\\u2013\\u2010\\u002d]{1,160}""".r
    def valid:Boolean = {
      recordVersion.forall(recordVersionPattern.matches(_)) &&
        dateOfChangePattern.matches(dateOfChange) &&
        schemeStructureOther.forall(schemeStructureOtherPattern.matches(_))
    }
  }

  val transformToETMPData: Reads[JsObject] = {

    def eventTypeNodes(events: Seq[JsObject]): JsObject = {
      val eventDetailNodes = events.foldLeft(Json.obj())((a, b) => a ++ b)
      if (events.isEmpty) Json.obj() else Json.obj("eventDetails" -> eventDetailNodes)
    }

    val schemeWindUp = (__ \ "schemeWindUpDate").readNullable[String].map {
      case Some(date) =>
        Some(
          Json.obj(
            "eventWindUp" -> Json.obj(
              "dateOfWindUp" -> date
            )
          )
        )
      case _ => None
    }

    val event13 = __.readNullable[Event13Data].map {
      case Some(data) =>
        if(data.valid) Some(
          Json.obj("event13" -> Json.toJsObject(data))
        )
        else throw new RuntimeException("Event 13 data invalid")
      case _ => None
    }

    val event18 = (__ \ "event18Confirmation").readNullable[Boolean].map {
      case Some(true) =>
        Some(
          Json.obj(
            "event18" -> Json.obj(
              "chargeablePmt" -> yes
            )
          )
        )
      case _ => None
    }

    for {
      ev13 <- event13
      ev18 <- event18
      schWindUp <- schemeWindUp
      header <- HeaderForAllAPIs.transformToETMPData()
    } yield {
      header ++ eventTypeNodes((ev13 ++ ev18 ++ schWindUp).toSeq)
    }
  }
}
