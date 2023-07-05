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

package repositories

import com.google.inject.{Inject, Singleton}
import com.mongodb.client.model.FindOneAndUpdateOptions
import models.GetDetailsCacheDataIdentifier
import models.enumeration.EventType
import org.joda.time.DateTime
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model._
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._
import play.api.mvc.Result
import play.api.mvc.Results.{NoContent, NotFound}
import play.api.{Configuration, Logging}
import repositories.GetDetailsCacheEntry.{eventTypeKey, expireAtKey, pstrKey, versionKey, yearKey}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.formats.MongoJodaFormats
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}

import java.time.{LocalDateTime, ZoneId}
import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}


case class GetDetailsCacheEntry(pstr: String, gdcdi: GetDetailsCacheDataIdentifier, data: JsValue, lastUpdated: LocalDateTime, expireAt: LocalDateTime)

object GetDetailsCacheEntry {
  implicit val format: Format[GetDetailsCacheEntry] = Json.format[GetDetailsCacheEntry]

  val pstrKey = "pstr"
  val eventTypeKey = "eventType"
  val yearKey = "year"
  val versionKey = "version"
  val expireAtKey = "expireAt"
  val lastUpdatedKey = "lastUpdated"
  val dataKey = "data"

  implicit val dateFormat: Format[DateTime] = MongoJodaFormats.dateTimeFormat

  implicit val formats: Format[GetDetailsCacheEntry] = new Format[GetDetailsCacheEntry] {
    override def writes(o: GetDetailsCacheEntry): JsValue = {
      Json.obj(
        "pstr" -> o.pstr
      ) ++ GetDetailsCacheDataIdentifier.formats.writes(o.gdcdi).as[JsObject]
    }

    override def reads(json: JsValue): JsResult[GetDetailsCacheEntry] = {
      (
        (JsPath \ "pstr").read[String] and
          (JsPath \ eventTypeKey).read[EventType](EventType.formats) and
          (JsPath \ yearKey).read[Int] and
          (JsPath \ versionKey).read[Int] and
          (JsPath \ dataKey).read[JsValue] and
          (JsPath \ lastUpdatedKey).read[LocalDateTime] and
          (JsPath \ expireAtKey).read[LocalDateTime]
        )(
        (pstr, eventType, year, version, data, lastUpdated, expireAt) =>
          GetDetailsCacheEntry(pstr, GetDetailsCacheDataIdentifier(eventType, year, version), data, lastUpdated, expireAt)
      ).reads(json)
    }
  }
}

@Singleton
class GetDetailsCacheRepository @Inject()(
                                            mongoComponent: MongoComponent,
                                            config: Configuration
                                          )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[GetDetailsCacheEntry](
    collectionName = config.underlying.getString("mongodb.get-details-cache-data.name"),
    mongoComponent = mongoComponent,
    domainFormat = GetDetailsCacheEntry.formats,
    indexes = Seq(
      IndexModel(
        Indexes.ascending(expireAtKey),
        IndexOptions().name("dataExpiry").expireAfter(0, TimeUnit.SECONDS).background(true)
      ),
      IndexModel(
        Indexes.ascending(pstrKey, eventTypeKey, yearKey, versionKey),
        IndexOptions().name(pstrKey + eventTypeKey + yearKey + versionKey).background(true).unique(true)
      )
    ),
    extraCodecs = Seq(
      Codecs.playFormatCodec(GetDetailsCacheDataIdentifier.formats),
      Codecs.playFormatCodec(EventType.formats)
    )
  ) with Logging {

  import GetDetailsCacheEntry._

  private val expireInSeconds = config.get[Int](path = "mongodb.get-details-cache-data.timeToLiveInSeconds")

  private def evaluatedExpireAt: LocalDateTime = {
    LocalDateTime.now(ZoneId.of("UTC")).plusSeconds(expireInSeconds)
  }

  def upsert(pstr: String, gdcdi: GetDetailsCacheDataIdentifier, data: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    val modifier = Updates.combine(
      Updates.set(pstrKey, pstr),
      Updates.set(eventTypeKey, gdcdi.eventType.toString),
      Updates.set(yearKey, gdcdi.year),
      Updates.set(versionKey, gdcdi.version),
      Updates.set(dataKey, Codecs.toBson(Json.toJson(data))),
      Updates.set(lastUpdatedKey, Codecs.toBson(LocalDateTime.now(ZoneId.of("UTC")))),
      Updates.set(expireAtKey, Codecs.toBson(evaluatedExpireAt))
    )
    val selector = Filters.and(
      Filters.equal(pstrKey, pstr),
      Filters.equal(eventTypeKey, gdcdi.eventType.toString),
      Filters.equal(yearKey, gdcdi.year),
      Filters.equal(versionKey, gdcdi.version)
    )
    collection.findOneAndUpdate(
      filter = selector,
      update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map(_ => ())
  }

  def get(pstr: String, gdcdi: GetDetailsCacheDataIdentifier)(implicit ec: ExecutionContext): Future[Option[JsValue]] = {
    collection.find[GetDetailsCacheEntry](
      Filters.and(
        Filters.equal(pstrKey, pstr),
        Filters.equal(eventTypeKey, gdcdi.eventType.toString),
        Filters.equal(yearKey, gdcdi.year),
        Filters.equal(versionKey, gdcdi.version)
      )
    ).headOption().map {
      _.map {
        dataEntry =>
          dataEntry.data
      }
    }
  }

  def remove(pstr: String, gdcdi: GetDetailsCacheDataIdentifier)(implicit ec: ExecutionContext): Future[Unit] = {
    collection.deleteOne(
      filter = Filters.and(
        Filters.equal(pstrKey, pstr),
        Filters.equal(eventTypeKey, gdcdi.eventType.toString),
        Filters.equal(yearKey, gdcdi.year),
        Filters.equal(versionKey, gdcdi.version)
      )
    ).toFuture().map(_ => true)
  }
}
