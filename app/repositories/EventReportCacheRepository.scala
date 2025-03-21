/*
 * Copyright 2024 HM Revenue & Customs
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
import crypto.DataEncryptor
import models.EventDataIdentifier
import models.enumeration.EventType
import models.enumeration.EventType.EventTypeNone
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model._
import org.mongodb.scala.result
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._
import play.api.{Configuration, Logging}
import repositories.EventReportCacheEntry.{eventTypeKey, expireAtKey, externalIdKey, pstrKey, versionKey, yearKey}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}

import java.time.{Instant, LocalDateTime, ZoneId}
import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}


case class EventReportCacheEntry(pstr: String, edi: EventDataIdentifier, data: JsValue, lastUpdated: LocalDateTime, expireAt: LocalDateTime)

object EventReportCacheEntry {
  implicit val format: Format[EventReportCacheEntry] = Json.format[EventReportCacheEntry]

  val externalIdKey = "externalId"
  val pstrKey = "pstr"
  val eventTypeKey = "eventType"
  val yearKey = "year"
  val versionKey = "version"
  val expireAtKey = "expireAt"
  val lastUpdatedKey = "lastUpdated"
  val dataKey = "data"

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
          (JsPath \ eventTypeKey).read[EventType](EventType.formats) and
          (JsPath \ yearKey).read[Int] and
          (JsPath \ versionKey).read[Int] and
          (JsPath \ dataKey).read[JsValue] and
          (JsPath \ lastUpdatedKey).read(localDateTimeReads).orElse(Reads.pure(LocalDateTime.now())) and
          (JsPath \ expireAtKey).read(localDateTimeReads).orElse(Reads.pure(LocalDateTime.now())) and
          (JsPath \ externalIdKey).read[String]
      )(
        (pstr, eventType, year, version, data, lastUpdated, expireAt, externalId) =>
            EventReportCacheEntry(pstr, EventDataIdentifier(eventType, year, version, externalId), data, lastUpdated, expireAt)
      ).reads(json)
    }
  }
}

@Singleton
class EventReportCacheRepository @Inject()(
                                            mongoComponent: MongoComponent,
                                            config: Configuration,
                                            cipher: DataEncryptor
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
        Indexes.ascending(pstrKey, eventTypeKey, yearKey, versionKey, externalIdKey),
        IndexOptions().name(pstrKey + eventTypeKey + yearKey + versionKey + externalIdKey).background(true).unique(true)
      )
    ),
    extraCodecs = Seq(
      Codecs.playFormatCodec(EventDataIdentifier.formats),
      Codecs.playFormatCodec(EventType.formats),
      Codecs.playFormatCodec(MongoJavatimeFormats.instantFormat)
    )
  ) with Logging {

  import EventReportCacheEntry._

  private val expireInSeconds = config.get[Int](path = "mongodb.event-reporting-data.timeToLiveInSeconds")
  private val nonEventTypeExpireInSeconds = config.get[Int](path = "mongodb.event-reporting-data.nonEventTypeTimeToLiveInSeconds")

  private def evaluatedExpireAt: Instant = {
    Instant.now().plusSeconds(expireInSeconds)
  }

  private def nonEventTypeEvaluatedExpireAt: Instant = {
    Instant.now().plusSeconds(nonEventTypeExpireInSeconds)
  }

  def upsert(pstr: String, edi: EventDataIdentifier, data: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    val modifier = Updates.combine(
      Updates.set(pstrKey, pstr),
      Updates.set(eventTypeKey, edi.eventType.toString),
      Updates.set(yearKey, edi.year),
      Updates.set(versionKey, edi.version),
      Updates.set(dataKey, Codecs.toBson(cipher.encrypt(pstr, Json.toJson(data)))),
      Updates.set(lastUpdatedKey, LocalDateTime.now(ZoneId.of("UTC"))),
      Updates.set(expireAtKey, evaluatedExpireAt)
    )
    def selector(pstr: String) = Filters.and(
      Filters.equal(pstrKey, pstr),
      Filters.equal(eventTypeKey, edi.eventType.toString),
      Filters.equal(yearKey, edi.year),
      Filters.equal(versionKey, edi.version),
      Filters.equal(externalIdKey, edi.externalId)
    )
    collection.findOneAndUpdate(
      filter = selector(pstr),
      update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map(_ => debugLog("save user answers", edi, pstr, data)).flatMap {
      case _ if !pstr.contains("_original_cache")=> updateExpire(selector(pstr + "_original_cache"), isEvent(edi.eventType)).toFuture().map { _ => () }
      case _ => Future.successful(())
    }
  }

  def changeVersion(externalId: String, pstr: String, version: Int, newVersion: Int)(implicit ec: ExecutionContext): Future[Option[result.UpdateResult]] = {
    val modifier = Updates.combine(
      Updates.set(versionKey, newVersion),
      Updates.set(lastUpdatedKey, LocalDateTime.now(ZoneId.of("UTC")))
    )
    val selector = Filters.and(
      Filters.equal(pstrKey, pstr),
      Filters.equal(versionKey, version),
      Filters.equal(externalIdKey, externalId)
    )

    collection.find(filter = selector).headOption().flatMap { foundItem =>
      if (foundItem.isDefined) {
        collection.updateMany(
          filter = selector,
          update = modifier).toFuture() .map(Some(_))
      } else {
        Future.successful(None)
      }

    }
  }

  private def debugLog(title:String, externalId:String, pstr: String, data: JsValue): Unit = {
    logger.debug(
      s"""$title
         |externalId: $externalId
         |PSTR: $pstr
         |Data:
         |${Json.prettyPrint(data)}
         |""".stripMargin)
  }

  private def debugLog(title:String, edi:EventDataIdentifier, pstr: String, data: JsValue): Unit = {
    logger.debug(
      s"""$title
         |edi:
         |${Json.prettyPrint(Json.toJson(edi))}
         |PSTR: $pstr
         |Data:
         |${Json.prettyPrint(data)}
         |""".stripMargin)
  }

  def upsert(externalId:String, pstr: String, data: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {

    val modifier = Updates.combine(
      Updates.set(externalIdKey, externalId),
      Updates.set(pstrKey, pstr),
      Updates.set(eventTypeKey, "None"),
      Updates.set(yearKey, 0),
      Updates.set(versionKey, 0),
      Updates.set(dataKey, Codecs.toBson(cipher.encrypt(pstr, Json.toJson(data)))),
      Updates.set(lastUpdatedKey, LocalDateTime.now(ZoneId.of("UTC"))),
      Updates.set(expireAtKey, nonEventTypeEvaluatedExpireAt)
    )
    def selector(pstr: String) = Filters.and(
      Filters.equal(pstrKey, pstr),
      Filters.equal(eventTypeKey, "None"),
      Filters.equal(yearKey, 0),
      Filters.equal(versionKey, 0),
      Filters.equal(externalIdKey, externalId)
    )

    collection.findOneAndUpdate(
      filter = selector(pstr),
      update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map(_ =>
      debugLog("save user answers", externalId, pstr, data)
    ).flatMap {
      case _ if !pstr.contains("_original_cache")=> updateExpire(selector(pstr + "_original_cache"), isEvent = false).toFuture().map { _ => () }
      case _ => Future.successful(())
    }
  }

  def getUserAnswers(externalId:String, pstr: String, optEventDataIdentifier: Option[EventDataIdentifier])
                    (implicit ec: ExecutionContext): Future[Option[JsObject]] = {
    optEventDataIdentifier match {
      case Some(edi) =>
        getByEDI(pstr, edi).map(_.map(_.as[JsObject]))
      case None =>
        getByEDI(pstr, EventDataIdentifier(EventType.EventTypeNone, 0, 0, externalId)).map(_.map(_.as[JsObject]))
    }
  }

  private def isEvent(eventType: EventType) = {
    eventType match {
      case EventType.EventTypeNone => false
      case _ => true
    }
  }

  private def getByEDI(pstr: String, edi: EventDataIdentifier)(implicit ec: ExecutionContext): Future[Option[JsValue]] = {
    def selector(pstr: String) = Filters.and(
      Filters.equal(pstrKey, pstr),
      Filters.equal(eventTypeKey, edi.eventType.toString),
      Filters.equal(yearKey, edi.year),
      Filters.equal(versionKey, edi.version),
      Filters.equal(externalIdKey, edi.externalId)
    )
    collection.find[EventReportCacheEntry](
      selector(pstr)
    ).headOption().map {
      _.map {
        dataEntry =>
          logger.warn(s"EventReportCacheRepository: Stored data is empty? ${dataEntry.data.as[JsObject].value.isEmpty}")
          val decryptedData = cipher.decrypt(pstr, dataEntry.data)
          debugLog("get user answers", edi, pstr, decryptedData)
          decryptedData
      }
    }.flatMap { resp =>
      for {
        u1 <- updateExpire(selector(pstr), isEvent(edi.eventType)).toFuture().map { _ => () }
        u2 <- if(!pstr.contains("_original_cache")) updateExpire(selector(pstr + "_original_cache"), isEvent(edi.eventType)).toFuture().map { _ => () } else Future.successful(())
      } yield {
        resp
      }
    }
  }

  private def updateExpire(selector: Bson, isEvent: Boolean) = {
    val modifier = Updates.set(expireAtKey, if(isEvent) evaluatedExpireAt else nonEventTypeEvaluatedExpireAt)

    collection.updateOne(selector, modifier)
  }

  def removeAllOnSignOut(externalId: String)(implicit ec: ExecutionContext): Future[Unit] = {
    collection.deleteMany(filterByKeys(Map("externalId" -> externalId))).toFuture().map { result =>
      logger.info(s"Removing all data from collection associated with ExternalId: $externalId")
      if (!result.wasAcknowledged) {
        logger.warn(s"Issue removing all data from collection associated with ExternalId: $externalId")
      }
      ()
    }
  }

  private def filterByKeys(mapOfKeys: Map[String, String]): Bson = {
    val filters = mapOfKeys.map(t => Filters.equal(t._1, t._2)).toList
    Filters.and(filters: _*)
  }

  def refreshExpire(externalId: String): Future[Boolean] = {
    for {
      u <- updateExpire(
        Filters.and(
          Filters.equal(externalIdKey, externalId),
          Filters.equal(eventTypeKey, EventTypeNone.toString)
        ),
        isEvent = false).toFuture()
      u2 <- updateExpire(
        Filters.and(
          Filters.equal(externalIdKey, externalId),
          Filters.notEqual(eventTypeKey, EventTypeNone.toString)
        ),
        isEvent = true).toFuture()
    } yield u.wasAcknowledged() && u2.wasAcknowledged()
  }
}
