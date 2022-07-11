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

import com.github.simplyscala.MongoEmbedDatabase
import com.typesafe.config.Config
import org.mockito.MockitoSugar
import org.mongodb.scala.model.Filters
import org.scalatest.concurrent.ScalaFutures.whenReady
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfter, BeforeAndAfterEach}
import play.api.Configuration
import play.api.libs.json.Json
import uk.gov.hmrc.mongo.MongoComponent

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class OverviewCacheRepositorySpec extends AnyWordSpec with MockitoSugar with Matchers with MongoEmbedDatabase with BeforeAndAfter with
  BeforeAndAfterEach { // scalastyle:off magic.number

  import OverviewCacheRepositorySpec._

  override def beforeEach: Unit = {
    super.beforeEach
    when(mockAppConfig.underlying).thenReturn(mockConfig)
    when(mockConfig.getString("mongodb.overview-cache.name")).thenReturn("overview-cache")
    when(mockConfig.getInt("mongodb.overview-cache.timeToLiveInSeconds")).thenReturn(ttlValue)
  }

  withEmbedMongoFixture(port = 24680) { _ =>
    "save" must {
      "save a new overview cache in Mongo collection when collection is empty" in {
        mongoCollectionDrop()

        val record = ("pstr-1", "eventType-1", "2022-07-09", "2023-07-09", Json.parse("""{"data":"1"}"""))
        val filters = Filters.and(Filters.eq("pstr", record._1), Filters.eq("eventType", record._2),
          Filters.eq("startDate", record._3), Filters.eq("endDate", record._4))

        val documentsInDB = for {
          _ <- overviewCacheRepository.save(record._1, record._2, record._3, record._4, record._5)
          documentsInDB <- overviewCacheRepository.collection.find[OverviewCacheEntry](filters).toFuture()
        } yield documentsInDB

        whenReady(documentsInDB) {
          documentsInDB =>
            documentsInDB.size mustBe 1
        }
      }

      "update an existing overview cache in Mongo collection" in {
        mongoCollectionDrop()

        val record1 = ("pstr-1", "eventType-1", "2022-07-09", "2023-07-09", Json.parse("""{"data":"1"}"""))
        val record2 = ("pstr-1", "eventType-1", "2022-07-09", "2023-07-09", Json.parse("""{"data":"2"}"""))
        val filters = Filters.and(Filters.eq("pstr", record1._1), Filters.eq("eventType", record1._2),
          Filters.eq("startDate", record1._3), Filters.eq("endDate", record1._4))

        val documentsInDB = for {
          _ <- overviewCacheRepository.save(record1._1, record1._2, record1._3, record1._4, record1._5)
          _ <- overviewCacheRepository.save(record2._1, record2._2, record2._3, record2._4, record2._5)
          documentsInDB <- overviewCacheRepository.collection.find[OverviewCacheEntry](filters).toFuture()
        } yield documentsInDB

        whenReady(documentsInDB) {
          documentsInDB =>
            documentsInDB.size mustBe 1
            documentsInDB.head.data mustBe record2._5
        }
      }

      "save a new overview cache in Mongo collection when one of filter is different" in {
        mongoCollectionDrop()

        val record1 = ("pstr-1", "eventType-1", "2022-07-09", "2023-07-09", Json.parse("""{"data":"1"}"""))
        val record2 = ("pstr-2", "eventType-1", "2022-07-09", "2023-07-09", Json.parse("""{"data":"2"}"""))

        val documentsInDB = for {
          _ <- overviewCacheRepository.save(record1._1, record1._2, record1._3, record1._4, record1._5)
          _ <- overviewCacheRepository.save(record2._1, record2._2, record2._3, record2._4, record2._5)
          documentsInDB <- overviewCacheRepository.collection.find[OverviewCacheEntry].toFuture()
        } yield documentsInDB

        whenReady(documentsInDB) {
          documentsInDB =>
            documentsInDB.size mustBe 2
        }
      }
    }

    "get" must {
      "retrieve existing overview cache in Mongo collection" in {
        mongoCollectionDrop()

        val record = ("pstr-1", "eventType-1", "2022-07-09", "2023-07-09", Json.parse("""{"data":"1"}"""))

        val documentsInDB = for {
          _ <- overviewCacheRepository.save(record._1, record._2, record._3, record._4, record._5)
          documentsInDB <- overviewCacheRepository.get(record._1, record._2, record._3, record._4)
        } yield documentsInDB

        whenReady(documentsInDB) { documentsInDB =>
          documentsInDB.isDefined mustBe true
        }
      }
    }
  }
}

object OverviewCacheRepositorySpec extends AnyWordSpec with MockitoSugar {

  import scala.concurrent.ExecutionContext.Implicits._

  private val mockAppConfig = mock[Configuration]
  private val mockConfig = mock[Config]
  private val databaseName = "pension-scheme-event-reporting"
  private val mongoUri: String = s"mongodb://127.0.0.1:27017/$databaseName?heartbeatFrequencyMS=1000&rm.failover=default"
  private val mongoComponent = MongoComponent(mongoUri)
  private val ttlValue = 1800

  private def mongoCollectionDrop(): Void = Await
    .result(overviewCacheRepository.collection.drop().toFuture(), Duration.Inf)

  def overviewCacheRepository: OverviewCacheRepository = new OverviewCacheRepository(mongoComponent, mockAppConfig)
}