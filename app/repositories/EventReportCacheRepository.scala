/*
 * Copyright 2022 HM Revenue & Customs
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
import models.enumeration.ApiTypes
import org.joda.time.{DateTime, DateTimeZone}
import org.mongodb.scala.bson.BsonBinary
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model._
import play.api.libs.json._
import play.api.{Configuration, Logging}
import repositories.EventReportCacheEntry.EventReportCacheEntryFormats.{apiTypesKey, expireAtKey, pstrKey}
import repositories.EventReportCacheEntry.{DataEntry, EventReportCacheEntry, EventReportCacheEntryFormats, JsonDataEntry}
import uk.gov.hmrc.crypto.{Crypted, CryptoWithKeysFromConfig, PlainText}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.formats.MongoBinaryFormats.{byteArrayReads, byteArrayWrites}
import uk.gov.hmrc.mongo.play.json.formats.MongoJodaFormats
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}

import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}


object EventReportCacheEntry {

  sealed trait EventReportCacheEntry

  case class DataEntry(pstr: String, apiTypes: String, data: BsonBinary, lastUpdated: DateTime, expireAt: DateTime)
    extends EventReportCacheEntry

  case class JsonDataEntry(pstr: String, apiTypes: String, data: JsValue, lastUpdated: DateTime, expireAt: DateTime)
    extends EventReportCacheEntry

  object DataEntry {
    def apply(pstr: String,
              apiTypes: ApiTypes,
              data: Array[Byte],
              lastUpdated: DateTime = DateTime.now(DateTimeZone.UTC),
              expireAt: DateTime): DataEntry = {

      DataEntry(pstr, apiTypes.toString, BsonBinary(data), lastUpdated, expireAt)
    }

    final val bsonBinaryReads: Reads[BsonBinary] = byteArrayReads.map(BsonBinary(_))
    final val bsonBinaryWrites: Writes[BsonBinary] = byteArrayWrites.contramap(_.getData)
    implicit val bsonBinaryFormat: Format[BsonBinary] = Format(bsonBinaryReads, bsonBinaryWrites)

    implicit val dateFormat: Format[DateTime] = MongoJodaFormats.dateTimeFormat
    implicit val format: Format[DataEntry] = Json.format[DataEntry]
  }

  object JsonDataEntry {
    def applyJsonDataEntry(pstr: String,
                           apiTypes: ApiTypes,
                           data: JsValue,
                           lastUpdated: DateTime = DateTime.now(DateTimeZone.UTC),
                           expireAt: DateTime): JsonDataEntry = {

      JsonDataEntry(pstr, apiTypes.toString, data, lastUpdated, expireAt)
    }

    implicit val dateFormat: Format[DateTime] = MongoJodaFormats.dateTimeFormat
    implicit val format: Format[JsonDataEntry] = Json.format[JsonDataEntry]
  }

  object EventReportCacheEntryFormats {
    implicit val dateFormat: Format[DateTime] = MongoJodaFormats.dateTimeFormat
    implicit val format: Format[EventReportCacheEntry] = Json.format[EventReportCacheEntry]

    val pstrKey = "pstr"
    val expireAtKey = "expireAt"
    val lastUpdatedKey = "lastUpdated"
    val apiTypesKey = "apiTypes"
    val dataKey = "data"

  }
}

@Singleton
class EventReportCacheRepository @Inject()(
                                            mongoComponent: MongoComponent,
                                            config: Configuration
                                          )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[EventReportCacheEntry](
    collectionName = config.underlying.getString("mongodb.pension-scheme-event-reporting-cache.event-reporting-data.name"),
    mongoComponent = mongoComponent,
    domainFormat = EventReportCacheEntryFormats.format,
    extraCodecs = Seq(
      Codecs.playFormatCodec(JsonDataEntry.format),
      Codecs.playFormatCodec(DataEntry.format)
    ),
    indexes = Seq(
      IndexModel(
        Indexes.ascending(expireAtKey),
        IndexOptions().name("dataExpiry").expireAfter(0, TimeUnit.SECONDS).background(true)
      ),
      IndexModel(
        Indexes.ascending(pstrKey),
        IndexOptions().name(pstrKey).background(true)
      ),
      IndexModel(
        Indexes.ascending(apiTypesKey),
        IndexOptions().name(apiTypesKey).background(true)
      )
    )
  ) with Logging {

  import EventReportCacheEntryFormats._

  private val encryptionKey: String = "event.json.encryption"
  private val encrypted: Boolean = config.getOptional[Boolean]("encrypted").getOrElse(true)
  private val jsonCrypto: CryptoWithKeysFromConfig = new CryptoWithKeysFromConfig(baseConfigKey = encryptionKey, config.underlying)
  private val expireInDays = config.get[Int](path = "mongodb.pension-scheme-event-reporting-cache.event-reporting-data.timeToLiveInDays")

  private def evaluatedExpireAt: DateTime = DateTime.now(DateTimeZone.UTC).toLocalDate.plusDays(expireInDays + 1).toDateTimeAtStartOfDay()

  def upsert(pstr: String, apiType: ApiTypes, data: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    if (encrypted) {
      val encryptedPstr = jsonCrypto.encrypt(PlainText(pstr)).value
      val unencrypted = PlainText(Json.stringify(Json.toJson(data)))
      val encryptedData = jsonCrypto.encrypt(unencrypted).value
      val dataAsByteArray: Array[Byte] = encryptedData.getBytes("UTF-8")

      val dataEntry = DataEntry(encryptedPstr, apiType, dataAsByteArray, expireAt = evaluatedExpireAt)

      val modifier = Updates.combine(
        Updates.set(pstrKey, dataEntry.pstr),
        Updates.set(apiTypesKey, dataEntry.apiTypes),
        Updates.set(dataKey,dataEntry.data),
        Updates.set(lastUpdatedKey, Codecs.toBson(dataEntry.lastUpdated)),
        Updates.set(expireAtKey, Codecs.toBson(dataEntry.expireAt))
      )
      val selector = Filters.and(Filters.equal(pstrKey, encryptedPstr), Filters.equal(apiTypesKey, dataEntry.apiTypes))
      collection.withDocumentClass[DataEntry]().findOneAndUpdate(
        filter = selector,
        update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map(_ => ())
    } else {
      val record = JsonDataEntry.applyJsonDataEntry(
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

      collection.withDocumentClass[JsonDataEntry]().findOneAndUpdate(
        filter = selector,
        update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map(_ => ())
    }
  }

  private def filterEncryptKeys(mapOfKeys: Map[String, String]): Bson = {
    val filters = mapOfKeys.map {
      case key if key._1 == pstrKey || key._1 == apiTypesKey =>
        val encryptedValue = jsonCrypto.encrypt(PlainText(key._2)).value
        Filters.equal(key._1, encryptedValue)
      case key => Filters.equal(key._1, key._2)
    }.toList
    Filters.and(filters: _*)
  }

  def getByKeys(mapOfKeys: Map[String, String])(implicit ec: ExecutionContext): Future[Option[JsValue]] = {
    if (encrypted) {
      collection.find[DataEntry](filterEncryptKeys(mapOfKeys)).headOption().map {
        _.map {
          dataEntry =>
            val dataAsString = new String(dataEntry.data.getData, StandardCharsets.UTF_8)
            val decrypted: PlainText = jsonCrypto.decrypt(Crypted(dataAsString))
            Json.parse(decrypted.value)
        }
      }
    }
    else {
      val filters = mapOfKeys.map(t => Filters.equal(t._1, t._2)).toList
      collection.find[JsonDataEntry](Filters.and(filters: _*)).headOption().map {
        _.map {
          dataEntry =>
            dataEntry.data
        }
      }
    }
  }

  def remove(mapOfKeys: Map[String, String])(implicit ec: ExecutionContext): Future[Boolean] = {
    val selector = if (encrypted) {
      filterEncryptKeys(mapOfKeys)
    } else {
      val filters = mapOfKeys.map(t => Filters.equal(t._1, t._2)).toList
      Filters.and(filters: _*)
    }
    collection.deleteOne(selector).toFuture().map { result =>
      logger.info(s"Removing row from collection $collectionName")
      result.wasAcknowledged
    }
  }
}
