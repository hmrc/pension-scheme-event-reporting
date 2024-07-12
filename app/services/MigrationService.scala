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

package services

import com.google.inject.Inject
import org.mongodb.scala.bson.{BsonDateTime, BsonDocument, BsonString}
import org.mongodb.scala.model.{Filters, Updates}
import play.api.Logging
import repositories.EventReportCacheEntry.expireAtKey
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.lock.{LockService, MongoLockRepository}

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

class MigrationService @Inject()(mongoLockRepository: MongoLockRepository,
                                 mongoComponent: MongoComponent)(implicit ec: ExecutionContext) extends Logging {
   private val lock = LockService(mongoLockRepository, "eventReportingEventLock_lock", Duration(10, TimeUnit.MINUTES))

  private def dropCollection(collectionName: String) = {
    val collection = mongoComponent.database.getCollection(collectionName)
    logger.warn("[PODS-9166] Started collection drop")
    collection.drop().toFuture()
  }


  lock withLock {
    for {
      res <- dropCollection("event-reporting-event-lock")
    } yield res
  } map {
    case Some(result) =>
      logger.warn(s"[PODS-9166] collection successfully dropped")
    case None => logger.warn(s"[PODS-9166] collection drop locked by other instance")
  } recover {
    case e => logger.error("Locking finished with error", e)
  }
}
