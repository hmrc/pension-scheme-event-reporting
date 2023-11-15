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

import models.EventDataIdentifier
import models.cache.EventLockJson
import models.enumeration.EventType
import org.mockito.Mockito.when
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}
import org.scalatestplus.mockito.MockitoSugar
import play.api.Configuration
import uk.gov.hmrc.mongo.MongoComponent

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EventLockRepositorySpec extends AnyWordSpec with MockitoSugar with Matchers
  with EmbeddedMongoDBSupport with BeforeAndAfter with BeforeAndAfterAll with ScalaFutures { // scalastyle:off magic.number

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds), Span(1, Millis))

  private val mockAppConfig = mock[Configuration]
  private val ttlValue = 28

  private def buildFormRepository(mongoHost: String, mongoPort: Int) = {
    val databaseName = "pension-scheme-event-reporting"
    val mongoUri = s"mongodb://$mongoHost:$mongoPort/$databaseName?heartbeatFrequencyMS=1000&rm.failover=default"
    new EventLockRepository(MongoComponent(mongoUri), mockAppConfig)
  }

  var eventLockRepository: EventLockRepository = _

  override def beforeAll(): Unit = {
    when(mockAppConfig.get[String]("mongodb.event-reporting-event-lock.name")).thenReturn("event-reporting-lock-data")
    when(mockAppConfig.get[Int]("mongodb.event-reporting-event-lock.timeToLiveInSeconds")).thenReturn(ttlValue)

    initMongoDExecutable()
    startMongoD()
    eventLockRepository = buildFormRepository(mongoHost, mongoPort)
  }

  override def afterAll(): Unit =
    stopMongoD()

  "upsertIfNotLocked" must {
    "create a lock if lock does not exist, creator should not see the lock" in {
      val edi = EventDataIdentifier(EventType.Event7, 1, 1, "externalId")
      eventLockRepository.collection.drop().toFuture()
      val result = for {
        _ <- eventLockRepository.collection.drop().toFuture()
        upsertResult <- eventLockRepository.upsertIfNotLocked(
          "pstr",
          "psaId",
          edi
        )
        upsertResult2 <- eventLockRepository.upsertIfNotLocked(
          "pstr2",
          "psaId",
          edi
        )
        lockedEvents <- eventLockRepository.getLockedEventTypes("pstr", "psaId", 1, 1, "externalId")
        event7IsLocked <- eventLockRepository.eventIsLocked("pstr", "psaId", edi)
        lockedEvents2 <- eventLockRepository.getLockedEventTypes("pstr2", "psaId", 1, 1, "externalId")
        event7IsLocked2 <- eventLockRepository.eventIsLocked("pstr2", "psaId", edi)
      } yield (upsertResult, upsertResult2, lockedEvents, event7IsLocked, lockedEvents2, event7IsLocked2)

      whenReady(result) { case (upsertResult, upsertResult2, lockedEvents, event7IsLocked, lockedEvents2, event7IsLocked2) =>
        lockedEvents.length mustBe 0
        event7IsLocked mustBe false
        lockedEvents2.length mustBe 0
        event7IsLocked2 mustBe false
        upsertResult mustBe true
        upsertResult2 mustBe true
      }
    }

    "show locked status to user who did not create a lock, locking criteria is either psaPspId or externalId difference" in {
      val edi = EventDataIdentifier(EventType.Event7, 1, 1, "externalId")
      val edi2 = EventDataIdentifier(EventType.Event7, 1, 1, "externalId2")
      val result = for {
        _ <- eventLockRepository.collection.drop().toFuture()
        _ <- eventLockRepository.upsertIfNotLocked(
          "pstr",
          "psaId",
          edi
        )
        lockedEvents <- eventLockRepository.getLockedEventTypes("pstr", "psaId2", 1, 1, "externalId")
        event7IsLocked <- eventLockRepository.eventIsLocked("pstr", "psaId", edi2)
      } yield lockedEvents -> event7IsLocked

      whenReady(result) { case (lockedEvents, event7IsLocked) =>
        lockedEvents mustBe Seq(EventType.Event7)
        event7IsLocked mustBe true
      }

    }

    "update expireAt value if lock already exists" in {
      val edi = EventDataIdentifier(EventType.Event7, 1, 1, "externalId")

      def upsert: Future[Boolean] = eventLockRepository.upsertIfNotLocked(
        "pstr",
        "psaId",
        edi
      )

      def find: Future[EventLockJson] = eventLockRepository.collection.find().toFuture().map(_.head)

      val result = for {
        _ <- eventLockRepository.collection.drop().toFuture()
        result <- upsert
        find1 <- find
        result2 <- upsert
        find2 <- find
      } yield (result, result2, find1, find2)

      whenReady(result) { case (result, result2, find1, find2) =>
        result mustBe true
        result2 mustBe true
        find1.expireAt.isBefore(find2.expireAt) mustBe true
      }
    }

    "return false if locked" in {
      val edi = EventDataIdentifier(EventType.Event7, 1, 1, "externalId")
      val edi2 = EventDataIdentifier(EventType.Event7, 1, 1, "externalId2")
      val result = for {
        _ <- eventLockRepository.collection.drop().toFuture()
        _ <- eventLockRepository.upsertIfNotLocked(
          "pstr",
          "psaId",
          edi
        )
        result <- eventLockRepository.upsertIfNotLocked(
          "pstr",
          "psaId",
          edi2
        )
        result2 <- eventLockRepository.upsertIfNotLocked(
          "pstr",
          "psaId2",
          edi
        )
      } yield result -> result2

      whenReady(result) { case (result, result2) =>
        result mustBe false
      }
    }
  }

  "getLockedEventTypes" must {
    "return locked events that were locked by other user" in {
      val result = for {
        _ <- eventLockRepository.collection.drop().toFuture()
        _ <- eventLockRepository.upsertIfNotLocked(
          "pstr",
          "psaId",
          EventDataIdentifier(EventType.Event7, 1, 1, "externalId")
        )
        _ <- eventLockRepository.upsertIfNotLocked(
          "pstr",
          "psaId",
          EventDataIdentifier(EventType.Event8, 1, 1, "externalId")
        )
        _ <- eventLockRepository.upsertIfNotLocked(
          "pstr",
          "psaId2",
          EventDataIdentifier(EventType.Event10, 1, 1, "externalId2")
        )
        lockedEventTypes <- eventLockRepository.getLockedEventTypes("pstr", "psaId2", 1, 1, "externalId2")
      } yield lockedEventTypes
      whenReady(result) { lockedEventTypes =>
        lockedEventTypes mustBe Seq(EventType.Event7, EventType.Event8)
      }
    }
  }
  "remove" must {
    "remove all event locks associated with the user's externalId" in {
      val result = for {
        _ <- eventLockRepository.collection.drop().toFuture()
        _ <- eventLockRepository.upsertIfNotLocked(
          "pstr",
          "psaId",
          EventDataIdentifier(EventType.Event7, 1, 1, "externalId")
        )
        _ <- eventLockRepository.upsertIfNotLocked(
          "pstr2",
          "psaId",
          EventDataIdentifier(EventType.Event8, 1, 1, "externalId")
        )
        _ <- eventLockRepository.upsertIfNotLocked(
          "pstr",
          "psaId2",
          EventDataIdentifier(EventType.Event10, 1, 1, "externalId2")
        )
        _ <- eventLockRepository.remove("externalId")
        allObjects <- eventLockRepository.collection.find().toFuture()
      } yield allObjects
      whenReady(result) { allObjects =>
        allObjects.length mustBe 1
      }
    }
  }

  "eventIsLocked" must {
    "return if event is locked for the user, not locked if user is owner of event lock" in {
      val result = for {
        _ <- eventLockRepository.collection.drop().toFuture()
        _ <- eventLockRepository.upsertIfNotLocked(
          "pstr",
          "psaId",
          EventDataIdentifier(EventType.Event7, 1, 1, "externalId")
        )
        locked <- eventLockRepository.eventIsLocked("pstr", "psaId",  EventDataIdentifier(EventType.Event7, 1, 1, "externalId"))
        locked2 <- eventLockRepository.eventIsLocked("pstr", "psaId2",  EventDataIdentifier(EventType.Event7, 1, 1, "externalId2"))
      } yield locked -> locked2
      whenReady(result) { case (locked, locked2) =>
        locked mustBe false
        locked2 mustBe true
      }
    }
  }
}
