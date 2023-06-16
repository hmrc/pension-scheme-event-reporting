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
import org.joda.time.{DateTime, DateTimeZone}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model._
import play.api.libs.json._
import play.api.{Configuration, Logging}
import repositories.OverviewCacheEntry.{endDateKey, expireAtKey, pstrKey, startDateKey}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.formats.MongoJodaFormats
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}

case class OverviewCacheEntry(pstr: String, data: JsValue, startDate: String, endDate: String, lastUpdated: DateTime, expireAt: DateTime)

object OverviewCacheEntry {

  def applyOverviewCacheEntry(pstr: String,
                              data: JsValue,
                              startDate: String,
                              endDate: String,
                              lastUpdated: DateTime = DateTime.now(DateTimeZone.UTC),
                              expireAt: DateTime): OverviewCacheEntry = {

    OverviewCacheEntry(pstr, data, startDate, endDate, lastUpdated, expireAt)
  }

  implicit val dateFormat: Format[DateTime] = MongoJodaFormats.dateTimeFormat
  implicit val format: Format[OverviewCacheEntry] = Json.format[OverviewCacheEntry]

  val pstrKey = "pstr"
  val startDateKey = "startDate"
  val endDateKey = "endDate"
  val expireAtKey = "expireAt"
  val lastUpdateKey = "lastUpdated"
  val dataKey = "data"
}

@Singleton
class OverviewCacheRepository @Inject()(
                                         mongoComponent: MongoComponent,
                                         config: Configuration
                                       )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[OverviewCacheEntry](
    collectionName = config.underlying.getString("mongodb.overview-cache.name"),
    mongoComponent = mongoComponent,
    domainFormat = OverviewCacheEntry.format,
    indexes = Seq(
      IndexModel(
        Indexes.ascending(expireAtKey),
        IndexOptions().name("dataExpiry").expireAfter(0, TimeUnit.SECONDS).background(true)
      ),
      IndexModel(
        Indexes.ascending(pstrKey, startDateKey, endDateKey),
        IndexOptions().name(s"${pstrKey}_${startDateKey}_${endDateKey}").unique(true).background(true)
      )
    )
  ) with Logging {

  import OverviewCacheEntry._

  private val expireInSeconds = config.underlying.getInt("mongodb.overview-cache.timeToLiveInSeconds")

  def upsert(pstr: String, startDate: String, endDate: String, data: JsValue)
            (implicit ec: ExecutionContext): Future[Unit] = {

    logger.info(s"Changes implemented in $collectionName cache")

    val upsertOptions = new FindOneAndUpdateOptions().upsert(true)

    val record = OverviewCacheEntry.applyOverviewCacheEntry(
      pstr = pstr, data = data, startDate, endDate, lastUpdated = DateTime.now(DateTimeZone.UTC),
      expireAt = DateTime.now(DateTimeZone.UTC).plusSeconds(expireInSeconds))

    collection.findOneAndUpdate(
      filter = filterByKeys(pstr, startDate, endDate),
      update = modifierByKeys(record), upsertOptions)
      .toFuture().map(_ => ())
  }

  def get(pstr: String, startDate: String, endDate: String)
         (implicit ec: ExecutionContext): Future[Option[JsValue]] = {
    logger.info(s"Retrieving data from $collectionName cache")
    collection.find[OverviewCacheEntry](filterByKeys(pstr, startDate, endDate))
      .headOption().map(optCacheEntry => optCacheEntry.map(cacheEntry => cacheEntry.data))
  }

  private def filterByKeys(pstr: String, startDate: String, endDate: String): Bson = {
    Filters.and(
      Filters.equal(pstrKey, pstr),
      Filters.equal(startDateKey, startDate),
      Filters.equal(endDateKey, endDate)
    )
  }

  private def modifierByKeys(record: OverviewCacheEntry): Bson = {
    Updates.combine(
      Updates.set(pstrKey, record.pstr),
      Updates.set(dataKey, Codecs.toBson(record.data)),
      Updates.set(startDateKey, record.startDate),
      Updates.set(endDateKey, record.endDate),
      Updates.set(lastUpdateKey, Codecs.toBson(record.lastUpdated)),
      Updates.set(expireAtKey, Codecs.toBson(record.expireAt))
    )
  }
}
