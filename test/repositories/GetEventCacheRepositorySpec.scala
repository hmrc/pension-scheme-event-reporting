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

class GetEventCacheRepositorySpec extends AnyWordSpec with MockitoSugar with Matchers with EmbeddedMongoDBSupport
  with BeforeAndAfter with BeforeAndAfterAll with ScalaFutures { // scalastyle:off magic.number

  import GetEventCacheRepositorySpec._

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds), Span(1, Millis))

  var getEventCacheRepository: GetEventCacheRepository = _

  override def beforeAll(): Unit = {
    when(mockAppConfig.underlying).thenReturn(mockConfig)
    when(mockConfig.getString("mongodb.get-event-cache.name")).thenReturn("get-event-cache")
    when(mockConfig.getInt("mongodb.get-event-cache.timeToLiveInSeconds")).thenReturn(ttlValue)

    initMongoDExecutable()
    startMongoD()
    getEventCacheRepository = buildFromRepository(mongoHost, mongoPort)
  }

  "upsert" must {
    "save a new event cache in Mongo collection when collection is empty" in {
      val record = ("pstr-1", "eventType-1", "2022-07-09", "1", Json.parse("""{"data":"1"}"""))
      val filters = Filters.and(Filters.eq("pstr", record._1), Filters.eq("eventType", record._2),
        Filters.eq("startDate", record._3), Filters.eq("version", record._4))

      val documentsInDB = for {
        _ <- getEventCacheRepository.collection.drop().toFuture()
        _ <- getEventCacheRepository.upsert(record._1, record._3, record._4, record._2, record._5)
        documentsInDB <- getEventCacheRepository.collection.find[GetEventCacheEntry](filters).toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 1
      }
    }

    "update an existing event cache in Mongo collection" in {

      val record1 = ("pstr-1", "eventType-1", "2022-07-09", "1", Json.parse("""{"data":"1"}"""))
      val record2 = ("pstr-1", "eventType-1", "2022-07-09", "1", Json.parse("""{"data":"2"}"""))
      val filters = Filters.and(Filters.eq("pstr", record1._1), Filters.eq("eventType", record1._2),
        Filters.eq("startDate", record1._3), Filters.eq("version", record1._4))

      val documentsInDB = for {
        _ <- getEventCacheRepository.collection.drop().toFuture()
        _ <- getEventCacheRepository.upsert(record1._1, record1._3, record1._4, record1._2, record1._5)
        _ <- getEventCacheRepository.upsert(record2._1, record2._3, record2._4, record2._2, record2._5)
        documentsInDB <- getEventCacheRepository.collection.find[GetEventCacheEntry](filters).toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 1
          documentsInDB.head.data mustBe record2._5
      }
    }
    //
    //    "save a new overview cache in Mongo collection when one of filter is different" in {
    //
    //      val record1 = ("pstr-1", "eventType-1", "2022-07-09", "2023-07-09", Json.parse("""{"data":"1"}"""))
    //      val record2 = ("pstr-2", "eventType-1", "2022-07-09", "2023-07-09", Json.parse("""{"data":"2"}"""))
    //
    //      val documentsInDB = for {
    //        _ <- overviewCacheRepository.collection.drop().toFuture()
    //        _ <- overviewCacheRepository.upsert(record1._1, record1._2, record1._3, record1._4, record1._5)
    //        _ <- overviewCacheRepository.upsert(record2._1, record2._2, record2._3, record2._4, record2._5)
    //        documentsInDB <- overviewCacheRepository.collection.find[OverviewCacheEntry]().toFuture()
    //      } yield documentsInDB
    //
    //      whenReady(documentsInDB) {
    //        documentsInDB =>
    //          documentsInDB.size mustBe 2
    //      }
    //    }
    //  }

    //  "get" must {
    //    "retrieve existing overview cache in Mongo collection" in {
    //
    //      val record = ("pstr-1", "eventType-1", "2022-07-09", "2023-07-09", Json.parse("""{"data":"1"}"""))
    //
    //      val documentsInDB = for {
    //        _ <- overviewCacheRepository.collection.drop().toFuture()
    //        _ <- overviewCacheRepository.upsert(record._1, record._2, record._3, record._4, record._5)
    //        documentsInDB <- overviewCacheRepository.get(record._1, record._2, record._3, record._4)
    //      } yield documentsInDB
    //
    //      whenReady(documentsInDB) { documentsInDB =>
    //        documentsInDB.isDefined mustBe true
    //      }
    //    }
    //  }
  }
}

object GetEventCacheRepositorySpec extends AnyWordSpec with MockitoSugar {

  private val mockAppConfig = mock[Configuration]
  private val mockConfig = mock[Config]
  private val ttlValue = 1800

  private def buildFromRepository(mongoHost: String, mongoPort: Int) = {
    val databaseName = "pension-scheme-event-reporting"
    val mongoUri = s"mongodb://$mongoHost:$mongoPort/$databaseName?heartbeatFrequencyMS=1000&rm.failover=default"
    new GetEventCacheRepository(MongoComponent(mongoUri), mockAppConfig)
  }
}

