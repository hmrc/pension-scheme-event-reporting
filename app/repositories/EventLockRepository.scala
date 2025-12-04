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

import com.google.inject.Inject
import models.EventDataIdentifier
import models.cache.EventLockJson
import models.enumeration.EventType
import org.mongodb.scala.model._
import org.mongodb.scala.result.DeleteResult
import org.mongodb.scala.model.Filters
import org.mongodb.scala._

import play.api.{Configuration, Logging}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats

import java.time.Instant
import java.util.concurrent.TimeUnit
import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EventLockRepository @Inject()(
                                           mongoComponent: MongoComponent,
                                           configuration: Configuration
                                         )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[EventLockJson](
    collectionName = configuration.get[String](path = "mongodb.event-reporting-event-lock.name"),
    mongoComponent = mongoComponent,
    domainFormat = implicitly,
    indexes = Seq(
      IndexModel(
        Indexes.ascending("psaOrPspId", "pstr", "edi.eventType", "edi.version", "edi.year"),
        IndexOptions().name("PsaOrPspId_Pstr_eventType_version_year_idx").unique(true).background(true)
      ),
      IndexModel(
        keys = Indexes.ascending("expireAt"),
        indexOptions = IndexOptions().name("dataExpiry").expireAfter(0, TimeUnit.SECONDS)
      )
    ),
    extraCodecs = Seq(
      Codecs.playFormatCodec(EventType.formats),
      Codecs.playFormatCodec(MongoJavatimeFormats.instantFormat),
      Codecs.playFormatCodec(EventDataIdentifier.formats)

    )
  ) with Logging {

  private val ttl = configuration.get[Int](path = "mongodb.event-reporting-event-lock.timeToLiveInSeconds")

  private def expireInSeconds: Instant = Instant.now()
    .plusSeconds(ttl)

  private lazy val documentExistsErrorCode = 11000

  /**
   *
   * @param pstr
   * @param psaOrPspId
   * @param edi
   * @return true if lock updated, false if locked
   */
  def upsertIfNotLocked(pstr: String, psaOrPspId: String, edi: EventDataIdentifier): Future[Boolean] = {
    collection.find(Filters.and(
      Filters.equal("pstr", pstr),
      Filters.equal("edi.year", edi.year),
      Filters.equal("edi.eventType", edi.eventType.toString),
      Filters.equal("edi.version", edi.version)
    )).toFuture().flatMap {
      case Seq() => insertEventLock(pstr, psaOrPspId, edi)
      case lockSeq if lockSeq.length > 1 => throw new RuntimeException("Multiple locks found")
      case Seq(lock) =>
        val lockedBySomeoneElse = lock.psaOrPspId != psaOrPspId || lock.edi.externalId != edi.externalId
        if(lockedBySomeoneElse) {
          Future.successful(false)
        } else {
          updateEventLockExpire(pstr, psaOrPspId, edi).toFuture().map { _ => true }
        }
      case _ => throw new RuntimeException("Unexpected match on upsertIfNotLocked")
    }
  }

  private def updateEventLockExpire(pstr: String, psaOrPspId: String, edi: EventDataIdentifier): SingleObservable[EventLockJson] = {
    collection.findOneAndReplace(
      filter = Filters.and(
        Filters.equal("pstr", pstr),
        Filters.equal("psaOrPspId", psaOrPspId),
        Filters.equal("edi.year", edi.year),
        Filters.equal("edi.eventType", edi.eventType.toString),
        Filters.equal("edi.version", edi.version),
        Filters.equal("edi.externalId", edi.externalId)
      ),
      replacement = EventLockJson(pstr, psaOrPspId, expireInSeconds, edi),
      options = new FindOneAndReplaceOptions().upsert(true)
    )
  }

  def getLockedEventTypes(pstr: String, psaOrPspId: String, year:Int, version:Int, externalId: String): Future[Seq[EventType]] = {
    collection.find(Filters.and(
      Filters.equal("pstr", pstr),
      Filters.equal("edi.year", year),
      Filters.gte("edi.version", version),
      Filters.or(
        Filters.notEqual("psaOrPspId", psaOrPspId),
        Filters.notEqual("edi.externalId", externalId)
      )
    )).toFuture().map { result =>
      if(result.nonEmpty) {
        val maxVersion = result.map(_.edi.version).max
        val currentVersionIsMaxVersion = version == maxVersion
        if (currentVersionIsMaxVersion) {
          val lockedEventTypes = result.map(_.edi.eventType)
          lockedEventTypes
        } else {
          Seq()
        }
      } else {
        Seq()
      }
    }
  }

  def remove(externalId: String): Future[DeleteResult] = {
    collection.deleteMany(Filters.equal("edi.externalId", externalId)).toFuture()
  }

  private def insertEventLock(pstr: String, psaOrPspId: String, edi: EventDataIdentifier): Future[Boolean] = {
    collection.insertOne(EventLockJson(pstr, psaOrPspId, expireInSeconds, edi)
    ).toFuture().map { _ => true }
      .recoverWith {
        case e: MongoWriteException if e.getCode == documentExistsErrorCode =>
          Future.successful(false)
      }
  }

  def eventIsLocked(pstr: String, psaOrPspId: String, edi: EventDataIdentifier): Future[Boolean] = {
    collection.find(Filters.and(
      Filters.equal("pstr", pstr),
      Filters.equal("edi.year", edi.year),
      Filters.equal("edi.eventType", edi.eventType.toString),
      Filters.equal("edi.version", edi.version),
      Filters.or(
        Filters.notEqual("psaOrPspId", psaOrPspId),
        Filters.notEqual("edi.externalId", edi.externalId)
      )
    )).toFuture().map(_.nonEmpty)
  }
}
