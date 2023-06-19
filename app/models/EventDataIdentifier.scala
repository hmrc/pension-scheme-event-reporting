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

import models.enumeration.ApiType
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{Format, JsPath, JsResult, JsString, JsValue, Json, Reads}

case class EventDataIdentifier(apiType: ApiType, year: Int, version: Int)

object EventDataIdentifier {
  implicit val formats: Format[EventDataIdentifier] = new Format[EventDataIdentifier] {
    override def writes(o: EventDataIdentifier): JsValue = {
      Json.obj(
        "apiType" -> o.apiType.toString,
        "year" -> o.year,
        "version" -> o.version
      )
    }

    override def reads(json: JsValue): JsResult[EventDataIdentifier] = {
      (
      (JsPath \ "apiType").read[ApiType](ApiType.formats) and
      (JsPath \ "year").read[Int] and
      (JsPath \ "version").read[Int]
        )(
        (apiType, year, version) => EventDataIdentifier(apiType, year, version)
      ).reads(json)
    }
  }
}
