/*
 * Copyright 2025 HM Revenue & Customs
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
import play.api.libs.json.{Format, JsObject, JsPath, JsResult, JsSuccess, JsValue, Json, Reads}
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats

import java.time.Instant

case class GetDetailsCacheEntry(pstr: String, gdcdi: GetDetailsCacheDataIdentifier, data: JsValue, lastUpdated: Instant, expireAt: Instant)

object GetDetailsCacheEntry {
  implicit val dateFormat: Format[Instant] = MongoJavatimeFormats.instantFormat

  implicit val format: Format[GetDetailsCacheEntry] = Json.format[GetDetailsCacheEntry]

  val PstrKey = "pstr"
  val EventTypeKey = "eventType"
  val YearKey = "year"
  val VersionKey = "version"
  val ExpireAtKey = "expireAt"
  val LastUpdatedKey = "lastUpdated"
  val DataKey = "data"

  //TODO: Most likely can just use MongoJavatimeFormats.instantReads.
  //TODO: Previously we've fixed an issue where date was stored as strings, which would not get picked up for expiry deletion in mongoDB.
  //TODO: We have performed migration in production environments. Everything should now be stored as Date objects.
  private val dateReads = new Reads[Instant] {
    def reads(json: JsValue): JsResult[Instant] = {
      val result = json.asOpt[String].map {
        Instant.parse
      }.getOrElse(json.as[Instant](MongoJavatimeFormats.instantReads))
      JsSuccess(result)
    }
  }

  implicit val formats: Format[GetDetailsCacheEntry] = new Format[GetDetailsCacheEntry] {
    override def writes(o: GetDetailsCacheEntry): JsValue = {
      Json.obj(
        "pstr" -> o.pstr
      ) ++ GetDetailsCacheDataIdentifier.formats.writes(o.gdcdi).as[JsObject]
    }

    override def reads(json: JsValue): JsResult[GetDetailsCacheEntry] = {
      (
        (JsPath \ "pstr").read[String] and
          (JsPath \ EventTypeKey).read[EventType](EventType.formats) and
          (JsPath \ YearKey).read[Int] and
          (JsPath \ VersionKey).read[Int] and
          (JsPath \ DataKey).read[JsValue] and
          (JsPath \ LastUpdatedKey).read[Instant](dateReads) and
          (JsPath \ ExpireAtKey).read[Instant](dateReads)
        )(
        (pstr, eventType, year, version, data, lastUpdated, expireAt) =>
          GetDetailsCacheEntry(pstr, GetDetailsCacheDataIdentifier(eventType, year, version), data, lastUpdated, expireAt)
      ).reads(json)
    }
  }
}