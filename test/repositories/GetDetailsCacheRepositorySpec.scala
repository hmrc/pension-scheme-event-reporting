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

import com.typesafe.config.Config
import models.GetDetailsCacheDataIdentifier
import models.enumeration.EventType
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

class GetDetailsCacheRepositorySpec extends AnyWordSpec with MockitoSugar with Matchers
  with EmbeddedMongoDBSupport with BeforeAndAfter with BeforeAndAfterAll with ScalaFutures { // scalastyle:off magic.number

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds), Span(1, Millis))

  import GetDetailsCacheRepositorySpec._

  var getDetailsCacheRepository: GetDetailsCacheRepository = _

  override def beforeAll(): Unit = {
    when(mockAppConfig.underlying).thenReturn(mockConfig)
    when(mockConfig.getString("mongodb.get-details-cache-data.name")).thenReturn("get-details-cache-data")
    when(mockAppConfig.get[Int]("mongodb.get-details-cache-data.timeToLiveInDays")).thenReturn(ttlValue)

    initMongoDExecutable()
    startMongoD()
    getDetailsCacheRepository = buildFormRepository(mongoHost, mongoPort)
  }

  private val pstrKey = "pstr"
  private val eventTypeKey = "eventType"
  private val yearKey = "year"
  private val versionKey = "version"
  private val pstr1 = "pstr-1"
  private val pstr2 = "pstr-2"
  private val pstr3 = "pstr-3"
  private val gdcdi = GetDetailsCacheDataIdentifier(EventType.Event22, 2020, 1)
  private val gdcdi2 = GetDetailsCacheDataIdentifier(EventType.Event22, 2020, 2)
  private val gdcdi3 = GetDetailsCacheDataIdentifier(EventType.Event22, 2020, 3)
  private val filters = searchFilter(pstr1, gdcdi)
  private val data1 = Json.parse("""{"data":"1"}""")
  private val data2 = Json.parse("""{"data":"2"}""")
  private val data3 = Json.parse("""{"data":"3"}""")

  private def searchFilter(pstr: String, gdcdi: GetDetailsCacheDataIdentifier) = {
    Filters.and(
      Filters.equal(pstrKey, pstr),
      Filters.equal(eventTypeKey, gdcdi.eventType.toString),
      Filters.equal(yearKey, gdcdi.year),
      Filters.equal(versionKey, gdcdi.version)
    )
  }

  override def afterAll(): Unit =
    stopMongoD()

  "upsert" must {
    "save a new item in Mongo collection when collection is empty" in {
      val data = Json.parse("""{"data":"1"}""")
      val documentsInDB = for {
        _ <- getDetailsCacheRepository.collection.drop().toFuture()
        _ <- getDetailsCacheRepository.upsert(pstr1, gdcdi, data)
        documentsInDB <- getDetailsCacheRepository.collection.find[GetDetailsCacheEntry](filters).toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 1
      }
    }

    "update an existing item in Mongo collection" in {
      val documentsInDB = for {
        _ <- getDetailsCacheRepository.collection.drop().toFuture()
        _ <- getDetailsCacheRepository.upsert(pstr1, gdcdi, data1)
        _ <- getDetailsCacheRepository.upsert(pstr1, gdcdi, data2)
        documentsInDB <- getDetailsCacheRepository.collection.find[GetDetailsCacheEntry](filters).toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 1
          documentsInDB.head.data mustBe data2
      }
    }

    "save a new item in Mongo collection when one of filter is different" in {
      val documentsInDB = for {
        _ <- getDetailsCacheRepository.collection.drop().toFuture()
        _ <- getDetailsCacheRepository.upsert(pstr1, gdcdi, data1)
        _ <- getDetailsCacheRepository.upsert(pstr2, gdcdi, data2)
        documentsInDB <- getDetailsCacheRepository.collection.find[GetDetailsCacheEntry]().toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 2
      }
    }
  }

  "remove" must {
    "remove all records for a given pstr without affecting other data" in {
      val documentsInDB = for {
        _ <- getDetailsCacheRepository.collection.drop().toFuture()
        _ <- getDetailsCacheRepository.upsert(pstr1, gdcdi, data1)
        _ <- getDetailsCacheRepository.upsert(pstr2, gdcdi2, data2)
        _ <- getDetailsCacheRepository.upsert(pstr3, gdcdi3, data3)
        _ <- getDetailsCacheRepository.remove(pstr1, gdcdi)
        documentsInDB <- getDetailsCacheRepository.collection.find[GetDetailsCacheEntry]().toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) { documentsInDB =>
        val doc1 = documentsInDB.head
        val doc2 = documentsInDB(1)
        documentsInDB.size mustBe 2
        (doc1.pstr, doc1.gdcdi.eventType, doc1.data) mustBe(pstr2, gdcdi2.eventType, data2)
        (doc2.pstr, doc2.gdcdi.eventType, doc2.data) mustBe(pstr3, gdcdi3.eventType, data3)
      }
    }
  }

  "get" must {
    "retrieve existing event report cache in Mongo collection when NO API type specified" in {
      val documentsInDB = for {
        _ <- getDetailsCacheRepository.collection.drop().toFuture()
        _ <- getDetailsCacheRepository.upsert(pstr1, gdcdi, data1)
        documentsInDB <- getDetailsCacheRepository.get(pstr1, gdcdi)
      } yield documentsInDB

      whenReady(documentsInDB) { documentsInDB =>
        documentsInDB.isDefined mustBe true
      }
    }

  }
}

object GetDetailsCacheRepositorySpec extends AnyWordSpec with MockitoSugar {

  private val mockAppConfig = mock[Configuration]
  private val mockConfig = mock[Config]
  private val ttlValue = 28

  private def buildFormRepository(mongoHost: String, mongoPort: Int) = {
    val databaseName = "pension-scheme-event-reporting"
    val mongoUri = s"mongodb://$mongoHost:$mongoPort/$databaseName?heartbeatFrequencyMS=1000&rm.failover=default"
    new GetDetailsCacheRepository(MongoComponent(mongoUri), mockAppConfig)
  }
}

