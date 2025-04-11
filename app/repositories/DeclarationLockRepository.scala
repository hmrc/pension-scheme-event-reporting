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
import models.cache.DeclarationLockJson
import org.mongodb.scala.MongoWriteException
import org.mongodb.scala.model._
import org.mongodb.scala.ObservableFuture
import play.api.{Configuration, Logging}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.PlayMongoRepository

import java.time.Instant
import java.util.concurrent.TimeUnit
import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class DeclarationLockRepository @Inject()(
                                           mongoComponent: MongoComponent,
                                           configuration: Configuration
                                         )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[DeclarationLockJson](
    collectionName = configuration.get[String](path = "mongodb.event-reporting-declaration-lock.name"),
    mongoComponent = mongoComponent,
    domainFormat = implicitly,
    indexes = Seq(
      IndexModel(
        Indexes.ascending("psaId", "pstr"),
        IndexOptions().name("PsaId_Pstr").unique(true).background(true)
      ),
      IndexModel(
        keys = Indexes.ascending("expireAt"),
        indexOptions = IndexOptions().name("dataExpiry").expireAfter(0, TimeUnit.SECONDS)
      )
    )
  ) with Logging {

  private def expireInSeconds: Instant = Instant.now().
    plusSeconds(configuration.get[Int](path = "mongodb.event-reporting-declaration-lock.timeToLiveInSeconds"))

  private lazy val documentExistsErrorCode = 11000

  def insertDoubleClickLock(pstr: String, psaId: String): Future[Boolean] = {

    collection.insertOne(DeclarationLockJson(pstr, psaId, expireInSeconds)
    ).toFuture().map { _ => true }
      .recoverWith {
        case e: MongoWriteException if e.getCode == documentExistsErrorCode =>
          Future.successful(false)
      }
  }
}
