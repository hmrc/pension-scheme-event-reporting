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

package models

import models.enumeration.EventType
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._

case class EventDataIdentifier(eventType: EventType, year: Int, version: Int, externalId:String)

object EventDataIdentifier {
  implicit val formats: Format[EventDataIdentifier] = new Format[EventDataIdentifier] {
    override def writes(o: EventDataIdentifier): JsValue = {
      Json.obj(
        "eventType" -> o.eventType.toString,
        "year" -> o.year,
        "version" -> o.version,
        "externalId" -> o.externalId
      )
    }

    override def reads(json: JsValue): JsResult[EventDataIdentifier] = {
      (
      (JsPath \ "eventType").read[EventType](EventType.formats) and
      (JsPath \ "year").read[Int] and
      (JsPath \ "version").read[Int] and
      (JsPath \ "externalId").read[String]
        )(
        (eventType, year, version, externalId) => EventDataIdentifier(eventType, year, version, externalId)
      ).reads(json)
    }
  }
}
