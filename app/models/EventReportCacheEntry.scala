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
import play.api.libs.json.{Format, JsObject, JsPath, JsResult, JsValue, Json, Reads}
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats

import java.time.{LocalDateTime, ZoneId}

case class EventReportCacheEntry(pstr: String, edi: EventDataIdentifier, data: JsValue, lastUpdated: LocalDateTime, expireAt: LocalDateTime)

object EventReportCacheEntry {
  implicit val format: Format[EventReportCacheEntry] = Json.format[EventReportCacheEntry]

  val ExternalIdKey = "externalId"
  val PstrKey = "pstr"
  val EventTypeKey = "eventType"
  val YearKey = "year"
  val VersionKey = "version"
  val ExpireAtKey = "expireAt"
  val LastUpdatedKey = "lastUpdated"
  val DataKey = "data"

  implicit val formats: Format[EventReportCacheEntry] = new Format[EventReportCacheEntry] {
    override def writes(o: EventReportCacheEntry): JsValue = {
      Json.obj(
        "pstr" -> o.pstr
      ) ++ EventDataIdentifier.formats.writes(o.edi).as[JsObject]
    }

    private val localDateTimeReads = MongoJavatimeFormats.instantReads.map(LocalDateTime.ofInstant(_, ZoneId.of("UTC")))

    //TODO: Most likely can remove ".orElse(Reads.pure(LocalDateTime.now()))" from date fields.
    //TODO: Previously we've fixed an issue where date was stored as strings, which would not get picked up for expiry deletion in mongoDB.
    //TODO: We have performed migration in production environments. Everything should now be stored as Date objects.
    override def reads(json: JsValue): JsResult[EventReportCacheEntry] = {
      (
        (JsPath \ "pstr").read[String] and
          (JsPath \ EventTypeKey).read[EventType](EventType.formats) and
          (JsPath \ YearKey).read[Int] and
          (JsPath \ VersionKey).read[Int] and
          (JsPath \ DataKey).read[JsValue] and
          (JsPath \ LastUpdatedKey).read(localDateTimeReads).orElse(Reads.pure(LocalDateTime.now())) and
          (JsPath \ ExpireAtKey).read(localDateTimeReads).orElse(Reads.pure(LocalDateTime.now())) and
          (JsPath \ ExternalIdKey).read[String]
      )(
        (pstr, eventType, year, version, data, lastUpdated, expireAt, externalId) =>
            EventReportCacheEntry(pstr, EventDataIdentifier(eventType, year, version, externalId), data, lastUpdated, expireAt)
      ).reads(json)
    }
  }
}