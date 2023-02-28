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

import com.typesafe.config.Config
import models.enumeration.ApiType.Api1826
import org.mockito.Mockito.when
import org.mongodb.scala.model.Filters
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}
import org.scalatestplus.mockito.MockitoSugar
import play.api.Configuration
import play.api.libs.json.Json
import uk.gov.hmrc.mongo.MongoComponent

import scala.concurrent.ExecutionContext.Implicits.global

class EventReportCacheRepositorySpec extends AnyWordSpec with MockitoSugar with Matchers
  with EmbeddedMongoDBSupport with BeforeAndAfter with BeforeAndAfterAll with ScalaFutures { // scalastyle:off magic.number

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds), Span(1, Millis))

  import EventReportCacheRepositorySpec._

  var eventReportCacheRepository: EventReportCacheRepository = _

  override def beforeAll(): Unit = {
    when(mockAppConfig.underlying).thenReturn(mockConfig)
    when(mockConfig.getString("mongodb.event-reporting-data.name")).thenReturn("event-reporting-data")
    when(mockAppConfig.get[Int]("mongodb.event-reporting-data.timeToLiveInDays")).thenReturn(ttlValue)

    initMongoDExecutable()
    startMongoD()
    eventReportCacheRepository = buildFormRepository(mongoHost, mongoPort)
  }

  override def afterAll(): Unit =
    stopMongoD()

  "upsert with event type" must {
    "save a new event report cache in Mongo collection when collection is empty" in {

      val record = ("pstr-1", Api1826, Json.parse("""{"data":"1"}"""))
      val filters = Filters.and(Filters.eq("pstr", record._1), Filters.eq("apiTypes", record._2.toString))

      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(record._1, record._2, record._3)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry](filters).toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 1
      }
    }

    "update an existing event report cache in Mongo collection" in {

      val record1 = ("pstr-1", Api1826, Json.parse("""{"data":"1"}"""))
      val record2 = ("pstr-1", Api1826, Json.parse("""{"data":"2"}"""))
      val filters = Filters.and(Filters.eq("pstr", record1._1), Filters.eq("apiTypes", record1._2.toString))

      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(record1._1, record1._2, record1._3)
        _ <- eventReportCacheRepository.upsert(record2._1, record2._2, record2._3)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry](filters).toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 1
          documentsInDB.head.data mustBe record2._3
      }
    }

    "save a new event report cache in Mongo collection when one of filter is different" in {

      val record1 = ("pstr-1", Api1826, Json.parse("""{"data":"1"}"""))
      val record2 = ("pstr-2", Api1826, Json.parse("""{"data":"2"}"""))

      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(record1._1, record1._2, record1._3)
        _ <- eventReportCacheRepository.upsert(record2._1, record2._2, record2._3)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry]().toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 2
      }
    }
  }

  "upsert with NO event type" must {
    val noEventType = "None"
    "save a new event report cache in Mongo collection when collection is empty" in {

      val record = ("pstr-1", noEventType, Json.parse("""{"data":"1"}"""))
      val filters = Filters.and(Filters.eq("pstr", record._1), Filters.eq("apiTypes", record._2))

      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(record._1, record._3)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry](filters).toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 1
      }
    }

    "update an existing event report cache in Mongo collection" in {

      val record1 = ("pstr-1", noEventType, Json.parse("""{"data":"1"}"""))
      val record2 = ("pstr-1", noEventType, Json.parse("""{"data":"2"}"""))
      val filters = Filters.and(Filters.eq("pstr", record1._1), Filters.eq("apiTypes", record1._2))

      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(record1._1, record1._3)
        _ <- eventReportCacheRepository.upsert(record2._1, record2._3)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry](filters).toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 1
          documentsInDB.head.data mustBe record2._3
      }
    }

    "save a new event report cache in Mongo collection when one of filter is different" in {

      val record1 = ("pstr-1", noEventType, Json.parse("""{"data":"1"}"""))
      val record2 = ("pstr-2", noEventType, Json.parse("""{"data":"2"}"""))

      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(record1._1, record1._3)
        _ <- eventReportCacheRepository.upsert(record2._1, record2._3)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry]().toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 2
      }
    }
  }

  "get" must {
    "retrieve existing event report cache in Mongo collection" in {

      val record = ("pstr-1", Api1826, Json.parse("""{"data":"1"}"""))

      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(record._1, record._2, record._3)
        documentsInDB <- eventReportCacheRepository.getByKeys(Map("pstr" -> record._1, "apiTypes" -> record._2.toString))
      } yield documentsInDB

      whenReady(documentsInDB) { documentsInDB =>
        documentsInDB.isDefined mustBe true
      }
    }
  }
}

object EventReportCacheRepositorySpec extends AnyWordSpec with MockitoSugar {

  private val mockAppConfig = mock[Configuration]
  private val mockConfig = mock[Config]
  private val ttlValue = 28

  private def buildFormRepository(mongoHost: String, mongoPort: Int) = {
    val databaseName = "pension-scheme-event-reporting"
    val mongoUri = s"mongodb://$mongoHost:$mongoPort/$databaseName?heartbeatFrequencyMS=1000&rm.failover=default"
    new EventReportCacheRepository(MongoComponent(mongoUri), mockAppConfig)
  }
}

