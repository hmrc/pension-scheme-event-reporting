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
import org.joda.time.{DateTime, DateTimeZone}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model._
import play.api.libs.json._
import play.api.{Configuration, Logging}
import repositories.GetEventCacheEntry._
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}
import uk.gov.hmrc.mongo.play.json.formats.MongoJodaFormats

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}

case class GetEventCacheEntry(pstr: String, eventType: String, data: JsValue, startDate: String, version: String, lastUpdated: DateTime, expireAt: DateTime)

object GetEventCacheEntry {

  def applyGetEventCacheEntry(pstr: String,
                              eventType: String,
                              data: JsValue,
                              startDate: String,
                              version: String,
                              lastUpdated: DateTime = DateTime.now(DateTimeZone.UTC),
                              expireAt: DateTime): GetEventCacheEntry = {

    GetEventCacheEntry(pstr, eventType, data, startDate, version, lastUpdated, expireAt)
  }

  implicit val dateFormat: Format[DateTime] = MongoJodaFormats.dateTimeFormat
  implicit val format: Format[GetEventCacheEntry] = Json.format[GetEventCacheEntry]

  val pstrKey = "pstr"
  val eventTypeKey = "eventType"
  val startDateKey = "startDate"
  val versionKey = "version"
  val expireAtKey = "expireAt"
  val lastUpdateKey = "lastUpdated"
  val dataKey = "data"
}

@Singleton
class GetEventCacheRepository @Inject()(
                                         mongoComponent: MongoComponent,
                                         config: Configuration
                                       )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[GetEventCacheEntry](
    collectionName = config.underlying.getString("mongodb.get-event-cache.name"),
    mongoComponent = mongoComponent,
    domainFormat = GetEventCacheEntry.format,
    indexes = Seq(
      IndexModel(
        Indexes.ascending(expireAtKey),
        IndexOptions().name("dataExpiry").expireAfter(0, TimeUnit.SECONDS).background(true)
      ),
      IndexModel(
        Indexes.ascending(pstrKey, eventTypeKey, startDateKey, versionKey),
        IndexOptions().name(s"${pstrKey}_${eventTypeKey}_${startDateKey}_${versionKey}").unique(true).background(true)
      )
    )
  ) with Logging {

  import GetEventCacheEntry._

  private val expireInSeconds = config.underlying.getInt("mongodb.get-event-cache.timeToLiveInSeconds")

  def upsert(pstr: String, startDate: String, version: String, eventType: String, data: JsValue)
            (implicit ec: ExecutionContext): Future[Unit] = {

    logger.info(s"Changes implemented in $collectionName cache")

    val upsertOptions = new FindOneAndUpdateOptions().upsert(true)

    val record = GetEventCacheEntry.applyGetEventCacheEntry(
      pstr = pstr, eventType = eventType, data = data, startDate, version, lastUpdated = DateTime.now(DateTimeZone.UTC),
      expireAt = DateTime.now(DateTimeZone.UTC).plusSeconds(expireInSeconds))

    collection.findOneAndUpdate(
      filter = filterByKeys(pstr, eventType, startDate, version),
      update = modifierByKeys(record), upsertOptions)
      .toFuture().map(_ => ())
  }

  def get(pstr: String, startDate: String, version: String, eventType: String)
         (implicit ec: ExecutionContext): Future[Option[JsValue]] = {
    logger.info(s"Retrieving data from $collectionName cache")
    collection.find[GetEventCacheEntry](filterByKeys(pstr, eventType, startDate, version))
      .headOption().map(optCacheEntry => optCacheEntry.map(cacheEntry => cacheEntry.data))
  }

  private def filterByKeys(pstr: String, eventType: String, startDate: String, version: String): Bson = {
    Filters.and(
      Filters.equal(pstrKey, pstr),
      Filters.equal(eventTypeKey, eventType),
      Filters.equal(startDateKey, startDate),
      Filters.equal(versionKey, version)
    )
  }

  private def modifierByKeys(record: GetEventCacheEntry): Bson = {
    Updates.combine(
      Updates.set(pstrKey, record.pstr),
      Updates.set(eventTypeKey, record.eventType),
      Updates.set(dataKey, Codecs.toBson(record.data)),
      Updates.set(startDateKey, record.startDate),
      Updates.set(versionKey, record.version),
      Updates.set(lastUpdateKey, Codecs.toBson(record.lastUpdated)),
      Updates.set(expireAtKey, Codecs.toBson(record.expireAt))
    )
  }
}
