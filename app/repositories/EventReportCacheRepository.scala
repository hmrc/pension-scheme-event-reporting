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
import models.enumeration.ApiType
import org.joda.time.{DateTime, DateTimeZone}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model._
import play.api.libs.json._
import play.api.{Configuration, Logging}
import repositories.EventReportCacheEntry.{apiTypesKey, expireAtKey, pstrKey}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.formats.MongoJodaFormats
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}


case class EventReportCacheEntry(pstr: String, apiTypes: String, data: JsValue, lastUpdated: DateTime, expireAt: DateTime)

object EventReportCacheEntry {

  def applyEventReportCacheEntry(pstr: String,
                                 apiTypes: ApiType,
                                 data: JsValue,
                                 lastUpdated: DateTime = DateTime.now(DateTimeZone.UTC),
                                 expireAt: DateTime): EventReportCacheEntry = {

    EventReportCacheEntry(pstr, apiTypes.toString, data, lastUpdated, expireAt)
  }

  implicit val dateFormat: Format[DateTime] = MongoJodaFormats.dateTimeFormat
  implicit val format: Format[EventReportCacheEntry] = Json.format[EventReportCacheEntry]

  val pstrKey = "pstr"
  val apiTypesKey = "apiTypes"
  val expireAtKey = "expireAt"
  val lastUpdatedKey = "lastUpdated"
  val dataKey = "data"
}

@Singleton
class EventReportCacheRepository @Inject()(
                                            mongoComponent: MongoComponent,
                                            config: Configuration
                                          )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[EventReportCacheEntry](
    collectionName = config.underlying.getString("mongodb.event-reporting-data.name"),
    mongoComponent = mongoComponent,
    domainFormat = EventReportCacheEntry.format,
    indexes = Seq(
      IndexModel(
        Indexes.ascending(expireAtKey),
        IndexOptions().name("dataExpiry").expireAfter(0, TimeUnit.SECONDS).background(true)
      ),
      IndexModel(
        Indexes.ascending(pstrKey, apiTypesKey),
        IndexOptions().name(pstrKey + apiTypesKey).background(true)
      )
    )
  ) with Logging {

  import EventReportCacheEntry._

  private val expireInDays = config.get[Int](path = "mongodb.event-reporting-data.timeToLiveInDays")

  private def evaluatedExpireAt: DateTime = DateTime.now(DateTimeZone.UTC).toLocalDate.plusDays(expireInDays + 1).toDateTimeAtStartOfDay()

  def upsert(pstr: String, apiType: ApiType, data: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    val record = EventReportCacheEntry.applyEventReportCacheEntry(
      pstr, apiType, Json.toJson(data),
      expireAt = evaluatedExpireAt)

    val modifier = Updates.combine(
      Updates.set(pstrKey, record.pstr),
      Updates.set(apiTypesKey, record.apiTypes),
      Updates.set(dataKey, Codecs.toBson(record.data)),
      Updates.set(lastUpdatedKey, Codecs.toBson(record.lastUpdated)),
      Updates.set(expireAtKey, Codecs.toBson(record.expireAt))
    )
    val selector = Filters.and(Filters.equal(pstrKey, record.pstr), Filters.equal(apiTypesKey, record.apiTypes))

    collection.findOneAndUpdate(
      filter = selector,
      update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map(_ => ())
  }

  def upsert(pstr: String, data: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    val lastUpdated = DateTime.now(DateTimeZone.UTC)
    val modifier = Updates.combine(
      Updates.set(pstrKey, pstr),
      Updates.set(apiTypesKey, "None"),
      Updates.set(dataKey, Codecs.toBson(Json.toJson(data))),
      Updates.set(lastUpdatedKey, Codecs.toBson(lastUpdated)),
      Updates.set(expireAtKey, Codecs.toBson(evaluatedExpireAt))
    )
    val selector = Filters.and(Filters.equal(pstrKey, pstr), Filters.equal(apiTypesKey, "None"))

    collection.findOneAndUpdate(
      filter = selector,
      update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map(_ => ())
  }

  def getUserAnswers(pstr: String, optApiType: Option[ApiType])(implicit ec: ExecutionContext): Future[Option[JsObject]] = {
    optApiType match {
      case Some(apiType) =>
        getByKeys(Map("pstr" -> pstr, "apiTypes" -> apiType.toString))
          .map(_.map(_.as[JsObject]))
      case None =>
        getByKeys(Map("pstr" -> pstr, "apiTypes" -> "None"))
          .map(_.map(_.as[JsObject]))
    }
  }

  private def getByKeys(mapOfKeys: Map[String, String])(implicit ec: ExecutionContext): Future[Option[JsValue]] = {
    collection.find[EventReportCacheEntry](filterByKeys(mapOfKeys)).headOption().map {
      _.map {
        dataEntry =>
          dataEntry.data
      }
    }
  }

  def remove(mapOfKeys: Map[String, String])(implicit ec: ExecutionContext): Future[Boolean] = {
    collection.deleteOne(filterByKeys(mapOfKeys)).toFuture().map { result =>
      logger.info(s"Removing row from collection $collectionName")
      result.wasAcknowledged
    }
  }

  def removeAllOnSignOut(pstr: String)(implicit ec: ExecutionContext): Future[Unit] = {
    collection.deleteMany(filterByKeys(Map("pstr" -> pstr))).toFuture().map { result =>
      logger.info(s"Removing all data from collection associated with $pstr")
      if (!result.wasAcknowledged) {
        logger.warn(s"Issue removing all data from collection associated with $pstr")
      }
      ()
    }
  }

  private def filterByKeys(mapOfKeys: Map[String, String]): Bson = {
    val filters = mapOfKeys.map(t => Filters.equal(t._1, t._2)).toList
    Filters.and(filters: _*)
  }
}
