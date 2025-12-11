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
import models.{GetDetailsCacheDataIdentifier, GetDetailsCacheEntry}
import models.enumeration.EventType
import models.GetDetailsCacheEntry.*
import org.mongodb.scala.model.*
import play.api.libs.json.*
import play.api.{Configuration, Logging}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GetDetailsCacheRepository @Inject()(
                                           mongoComponent: MongoComponent,
                                           config: Configuration,
                                           cipher: DataEncryptor
                                         )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[GetDetailsCacheEntry](
    collectionName = config.underlying.getString("mongodb.get-details-cache-data.name"),
    mongoComponent = mongoComponent,
    domainFormat = GetDetailsCacheEntry.formats,
    indexes = Seq(
      IndexModel(
        Indexes.ascending(ExpireAtKey),
        IndexOptions().name("dataExpiry").expireAfter(0, TimeUnit.SECONDS).background(true)
      ),
      IndexModel(
        Indexes.ascending(PstrKey, EventTypeKey, YearKey, VersionKey),
        IndexOptions().name(PstrKey + EventTypeKey + YearKey + VersionKey).background(true).unique(true)
      )
    ),
    extraCodecs = Seq(
      Codecs.playFormatCodec(GetDetailsCacheDataIdentifier.formats),
      Codecs.playFormatCodec(EventType.formats),
      Codecs.playFormatCodec(MongoJavatimeFormats.instantFormat)
    )
  ) with Logging {

  private val expireInSeconds = config.get[Int](path = "mongodb.get-details-cache-data.timeToLiveInSeconds")

  private def evaluatedExpireAt: Instant = {
    Instant.now().plusSeconds(expireInSeconds)
  }

  def upsert(pstr: String, gdcdi: GetDetailsCacheDataIdentifier, data: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    val modifier = Updates.combine(
      Updates.set(PstrKey, pstr),
      Updates.set(EventTypeKey, gdcdi.eventType.toString),
      Updates.set(YearKey, gdcdi.year),
      Updates.set(VersionKey, gdcdi.version),
      Updates.set(DataKey, Codecs.toBson(cipher.encrypt(pstr, Json.toJson(data)))),
      Updates.set(LastUpdatedKey, Codecs.toBson(Instant.now())),
      Updates.set(ExpireAtKey, evaluatedExpireAt)
    )
    val selector = Filters.and(
      Filters.equal(PstrKey, pstr),
      Filters.equal(EventTypeKey, gdcdi.eventType.toString),
      Filters.equal(YearKey, gdcdi.year),
      Filters.equal(VersionKey, gdcdi.version)
    )
    collection.findOneAndUpdate(
      filter = selector,
      update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map(_ => ())
  }

  def get(pstr: String, gdcdi: GetDetailsCacheDataIdentifier)(implicit ec: ExecutionContext): Future[Option[JsValue]] = {
    collection.find[GetDetailsCacheEntry](
      Filters.and(
        Filters.equal(PstrKey, pstr),
        Filters.equal(EventTypeKey, gdcdi.eventType.toString),
        Filters.equal(YearKey, gdcdi.year),
        Filters.equal(VersionKey, gdcdi.version)
      )
    ).headOption().map {
      _.map {
        dataEntry =>
          cipher.decrypt(pstr, dataEntry.data)
      }
    }
  }

  def remove(pstr: String, gdcdi: GetDetailsCacheDataIdentifier)(implicit ec: ExecutionContext): Future[Unit] = {
    collection.deleteOne(
      filter = Filters.and(
        Filters.equal(PstrKey, pstr),
        Filters.equal(EventTypeKey, gdcdi.eventType.toString),
        Filters.equal(YearKey, gdcdi.year),
        Filters.equal(VersionKey, gdcdi.version)
      )
    ).toFuture().map(_ => ())
  }
}
