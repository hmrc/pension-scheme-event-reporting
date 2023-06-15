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
import models.EventDataIdentifier
import models.enumeration.ApiType
import org.joda.time.{DateTime, DateTimeZone}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model._
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._
import play.api.{Configuration, Logging}
import repositories.EventReportCacheEntry.{apiTypesKey, expireAtKey, pstrKey, versionKey, yearKey}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.formats.MongoJodaFormats
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}

import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}


case class EventReportCacheEntry(pstr: String, edi: EventDataIdentifier, data: JsValue, lastUpdated: DateTime, expireAt: DateTime)

object EventReportCacheEntry {
  val pstrKey = "pstr"
  val apiTypesKey = "apiType"
  val yearKey = "year"
  val versionKey = "version"
  val expireAtKey = "expireAt"
  val lastUpdatedKey = "lastUpdated"
  val dataKey = "data"

  implicit val dateFormat: Format[DateTime] = MongoJodaFormats.dateTimeFormat

  implicit val formats: Format[EventReportCacheEntry] = new Format[EventReportCacheEntry] {
    override def writes(o: EventReportCacheEntry): JsValue = {
      Json.obj(
        "pstr" -> o.pstr
      ) ++ EventDataIdentifier.formats.writes(o.edi).as[JsObject]
    }

    override def reads(json: JsValue): JsResult[EventReportCacheEntry] = {
      (
        (JsPath \ "pstr").read[String] and
          (JsPath \ apiTypesKey).read[ApiType](ApiType.formats) and
          (JsPath \ yearKey).read[Int] and
          (JsPath \ versionKey).read[Int] and
          (JsPath \ dataKey).read[JsValue] and
          (JsPath \ lastUpdatedKey).read[DateTime] and
          (JsPath \ expireAtKey).read[DateTime]
        )(
        (pstr, apiType, year, version, data, lastUpdated, expireAt) =>
          EventReportCacheEntry(pstr, EventDataIdentifier(apiType, year, version), data, lastUpdated, expireAt)
      ).reads(json)
    }
  }
}

@Singleton
class EventReportCacheRepository @Inject()(
                                            mongoComponent: MongoComponent,
                                            config: Configuration
                                          )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[EventReportCacheEntry](
    collectionName = config.underlying.getString("mongodb.event-reporting-data.name"),
    mongoComponent = mongoComponent,
    domainFormat = EventReportCacheEntry.formats,
    indexes = Seq(
      IndexModel(
        Indexes.ascending(expireAtKey),
        IndexOptions().name("dataExpiry").expireAfter(0, TimeUnit.SECONDS).background(true)
      ),
      IndexModel(
        Indexes.ascending(pstrKey, apiTypesKey, yearKey, versionKey),
        IndexOptions().name(pstrKey + apiTypesKey + yearKey + versionKey).background(true).unique(true)
      )
    ),
    extraCodecs = Seq(
      Codecs.playFormatCodec(EventDataIdentifier.formats),
      Codecs.playFormatCodec(ApiType.formats)
    )
  ) with Logging {

  import EventReportCacheEntry._

  private val expireInDays = config.get[Int](path = "mongodb.event-reporting-data.timeToLiveInDays")

  private def evaluatedExpireAt: DateTime = DateTime.now(DateTimeZone.UTC).toLocalDate.plusDays(expireInDays + 1).toDateTimeAtStartOfDay()

  def upsert(pstr: String, edi: EventDataIdentifier, data: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    val modifier = Updates.combine(
      Updates.set(pstrKey, pstr),
      Updates.set(apiTypesKey, edi.apiType.toString),
      Updates.set(yearKey, edi.year),
      Updates.set(versionKey, edi.version),
      Updates.set(dataKey, Codecs.toBson(Json.toJson(data))),
      Updates.set(lastUpdatedKey, Codecs.toBson(DateTime.now(DateTimeZone.UTC))),
      Updates.set(expireAtKey, Codecs.toBson(evaluatedExpireAt))
    )
    val selector = Filters.and(
      Filters.equal(pstrKey, pstr),
      Filters.equal(apiTypesKey, edi.apiType.toString),
      Filters.equal(yearKey, edi.year),
      Filters.equal(versionKey, edi.version)
    )
    collection.findOneAndUpdate(
      filter = selector,
      update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map(_ => ())
  }

  def upsert(pstr: String, data: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    val modifier = Updates.combine(
      Updates.set(pstrKey, pstr),
      Updates.set(apiTypesKey, "None"),
      Updates.set(yearKey, 0),
      Updates.set(versionKey, 0),
      Updates.set(dataKey, Codecs.toBson(Json.toJson(data))),
      Updates.set(lastUpdatedKey, Codecs.toBson(DateTime.now(DateTimeZone.UTC))),
      Updates.set(expireAtKey, Codecs.toBson(evaluatedExpireAt))
    )
    val selector = Filters.and(Filters.equal(pstrKey, pstr), Filters.equal(apiTypesKey, "None"))

    collection.findOneAndUpdate(
      filter = selector,
      update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map(_ => ())
  }

  def getUserAnswers(pstr: String, optEventDataIdentifier: Option[EventDataIdentifier])(implicit ec: ExecutionContext): Future[Option[JsObject]] = {
    optEventDataIdentifier match {
      case Some(edi) => getByEDI(pstr, edi).map(_.map(_.as[JsObject]))
      case None =>
        getByKeys(Map("pstr" -> pstr, "apiType" -> ApiType.ApiNone.toString))
          .map(_.map(_.as[JsObject]))
    }
  }

  private def getByEDI(pstr: String, edi: EventDataIdentifier)(implicit ec: ExecutionContext): Future[Option[JsValue]] = {
    collection.find[EventReportCacheEntry](
      Filters.and(
        Filters.equal(pstrKey, pstr),
        Filters.equal(apiTypesKey, edi.apiType.toString),
        Filters.equal(yearKey, edi.year),
        Filters.equal(versionKey, edi.version)
      )
    ).headOption().map {
      _.map {
        dataEntry =>
          dataEntry.data
      }
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
