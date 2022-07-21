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
import models.enumeration.ApiType.Api1826
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

class EventReportCacheRepositorySpec extends AnyWordSpec with MockitoSugar with Matchers with MongoEmbedDatabase with BeforeAndAfter with
  BeforeAndAfterEach { // scalastyle:off magic.number

  import EventReportCacheRepositorySpec._

  override def beforeEach: Unit = {
    super.beforeEach
    when(mockAppConfig.underlying).thenReturn(mockConfig)
    when(mockConfig.getString("mongodb.event-reporting-data.name")).thenReturn("event-reporting-data")
    when(mockAppConfig.get[Int]("mongodb.event-reporting-data.timeToLiveInDays")).thenReturn(ttlValue)
  }

  withEmbedMongoFixture(port = 24680) { _ =>
    "upsert" must {
      "save a new event report cache in Mongo collection when collection is empty" in {
        mongoCollectionDrop()

        val record = ("pstr-1", Api1826, Json.parse("""{"data":"1"}"""))
        val filters = Filters.and(Filters.eq("pstr", record._1), Filters.eq("apiTypes", record._2.toString))

        val documentsInDB = for {
          _ <- eventReportCacheRepository.upsert(record._1, record._2, record._3)
          documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry](filters).toFuture()
        } yield documentsInDB

        whenReady(documentsInDB) {
          documentsInDB =>
            documentsInDB.size mustBe 1
        }
      }

      "update an existing event report cache in Mongo collection" in {
        mongoCollectionDrop()

        val record1 = ("pstr-1", Api1826, Json.parse("""{"data":"1"}"""))
        val record2 = ("pstr-1", Api1826, Json.parse("""{"data":"2"}"""))
        val filters = Filters.and(Filters.eq("pstr", record1._1), Filters.eq("apiTypes", record1._2.toString))

        val documentsInDB = for {
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
        mongoCollectionDrop()

        val record1 = ("pstr-1", Api1826, Json.parse("""{"data":"1"}"""))
        val record2 = ("pstr-2", Api1826, Json.parse("""{"data":"2"}"""))

        val documentsInDB = for {
          _ <- eventReportCacheRepository.upsert(record1._1, record1._2, record1._3)
          _ <- eventReportCacheRepository.upsert(record2._1, record2._2, record2._3)
          documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry].toFuture()
        } yield documentsInDB

        whenReady(documentsInDB) {
          documentsInDB =>
            documentsInDB.size mustBe 2
        }
      }
    }

    "get" must {
      "retrieve existing event report cache in Mongo collection" in {
        mongoCollectionDrop()

        val record = ("pstr-1", Api1826, Json.parse("""{"data":"1"}"""))

        val documentsInDB = for {
          _ <- eventReportCacheRepository.upsert(record._1, record._2, record._3)
          documentsInDB <- eventReportCacheRepository.getByKeys(Map("pstr" -> record._1, "apiTypes" -> record._2.toString))
        } yield documentsInDB

        whenReady(documentsInDB) { documentsInDB =>
          documentsInDB.isDefined mustBe true
        }
      }
    }
  }
}

object EventReportCacheRepositorySpec extends AnyWordSpec with MockitoSugar {

  import scala.concurrent.ExecutionContext.Implicits._

  private val mockAppConfig = mock[Configuration]
  private val mockConfig = mock[Config]
  private val databaseName = "pension-scheme-event-reporting"
  private val mongoUri: String = s"mongodb://127.0.0.1:27017/$databaseName?heartbeatFrequencyMS=1000&rm.failover=default"
  private val mongoComponent = MongoComponent(mongoUri)
  private val ttlValue = 28

  private def mongoCollectionDrop(): Void = Await
    .result(eventReportCacheRepository.collection.drop().toFuture(), Duration.Inf)

  def eventReportCacheRepository: EventReportCacheRepository = new EventReportCacheRepository(mongoComponent, mockAppConfig)
}

