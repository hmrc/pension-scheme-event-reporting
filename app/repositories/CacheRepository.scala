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
import com.mongodb.client.model.FindOneAndUpdateOptions
import org.mongodb.scala.model.Updates.set
import org.mongodb.scala.model._
import play.api.Logging
import play.api.libs.json._
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}

import java.time.{LocalDateTime, ZoneId}
import java.util.concurrent.TimeUnit
import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CacheRepository @Inject()(collectionName: String,
                                expireInSeconds: Option[Int] = None,
                                expireInDays: Option[Int] = None,
                                mongoComponent: MongoComponent
                               )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[JsValue](
    collectionName = collectionName,
    mongoComponent = mongoComponent,
    domainFormat = implicitly,
    indexes = repositories.CacheRepository.collectionIndexes,
    extraCodecs = Seq(
      Codecs.playFormatCodec(MongoJavatimeFormats.instantFormat)
    )
  ) with Logging {

  import CacheRepository._

  private def getExpireAt: LocalDateTime =
    (expireInSeconds, expireInDays) match {
      case (Some(seconds), None) =>
        LocalDateTime.now(ZoneId.of("UTC"))
          .plusSeconds(seconds)
      case (None, Some(days)) =>
        LocalDateTime.now(ZoneId.of("UTC"))
          .toLocalDate
          .plusDays(days).atStartOfDay
      case _ => throw new RuntimeException("Missing config item for expire in days/ seconds: one and only one should be present")
    }

  def save(id: String, userData: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    val upsertOptions = new FindOneAndUpdateOptions().upsert(true)
    collection.findOneAndUpdate(
      filter = Filters.eq(IdKey, id),
      update = Updates.combine(
        set(IdKey, id),
        set(DataKey, Codecs.toBson(userData)),
        set(LastUpdatedKey, Codecs.toBson(LocalDateTime.now(ZoneId.of("UTC")))),
        set(ExpireAtKey, getExpireAt)
      ),
      upsertOptions
    ).toFuture().map(_ => (): Unit)
  }

  def get(id: String)(implicit ec: ExecutionContext): Future[Option[JsValue]] = {
    collection.find(
      filter = Filters.eq(IdKey, id)
    ).toFuture().map {
      _.headOption.map { jsValue =>
        (jsValue \ DataKey).as[JsValue]
      }
    }
  }

  def remove(id: String)(implicit ec: ExecutionContext): Future[Boolean] = {
    collection.deleteOne(
      filter = Filters.eq(IdKey, id)
    ).toFuture().map(_ => true)
  }
}


object CacheRepository {
  private val IdKey = "id"
  private val DataKey = "data"
  private val ExpireAtKey = "expireAt"
  private val LastUpdatedKey = "lastUpdated"

  private val collectionIndexes: Seq[IndexModel] = Seq(
    IndexModel(
      keys = Indexes.ascending(IdKey),
      indexOptions = IndexOptions().name(IdKey).unique(true).background(true)
    ),
    IndexModel(
      keys = Indexes.ascending(ExpireAtKey),
      indexOptions = IndexOptions().name("dataExpiry")
        .expireAfter(0, TimeUnit.SECONDS)
        .background(true)
    )
  )
}