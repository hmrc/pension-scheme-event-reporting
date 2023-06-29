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
import models.EventDataIdentifier
import models.enumeration.ApiType.Api1826
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
import play.api.http.Status.{NOT_FOUND, NO_CONTENT}
import play.api.libs.json.Json
import uk.gov.hmrc.mongo.MongoComponent

import scala.concurrent.ExecutionContext.Implicits.global

class EventReportCacheRepositorySpec extends AnyWordSpec with MockitoSugar with Matchers
  with EmbeddedMongoDBSupport with BeforeAndAfter with BeforeAndAfterAll with ScalaFutures { // scalastyle:off magic.number

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds), Span(1, Millis))

  import EventReportCacheRepositorySpec._

  private val externalId = "externalId"
  var eventReportCacheRepository: EventReportCacheRepository = _

  override def beforeAll(): Unit = {
    when(mockAppConfig.underlying).thenReturn(mockConfig)
    when(mockConfig.getString("mongodb.event-reporting-data.name")).thenReturn("event-reporting-data")
    when(mockAppConfig.get[Int]("mongodb.event-reporting-data.timeToLiveInDays")).thenReturn(ttlValue)

    initMongoDExecutable()
    startMongoD()
    eventReportCacheRepository = buildFormRepository(mongoHost, mongoPort)
  }

  private val pstrKey = "pstr"
  private val eventTypeKey = "eventType"
  private val yearKey = "year"
  private val versionKey = "version"
  private val pstr1 = "pstr-1"
  private val pstr2 = "pstr-2"
  private val pstr3 = "pstr-3"
  private val edi = EventDataIdentifier(EventType.Event22, 2020, 1, externalId)
  private val edi2 = EventDataIdentifier(EventType.Event22, 2020, 2, externalId)
  private val edi3 = EventDataIdentifier(EventType.Event22, 2020, 3, externalId + "other")
  private val filters = searchFilter(pstr1, edi)
  private val data1 = Json.parse("""{"data":"1"}""")
  private val data2 = Json.parse("""{"data":"2"}""")
  private val data3 = Json.parse("""{"data":"3"}""")

  private def searchFilter(pstr: String, edi: EventDataIdentifier) = {

    Filters.and(
      Filters.equal(pstrKey, pstr),
      Filters.equal(eventTypeKey, edi.eventType.toString),
      Filters.equal(yearKey, edi.year),
      Filters.equal(versionKey, edi.version)
    )
  }

  override def afterAll(): Unit =
    stopMongoD()

  "upsert with event type" must {
    "save a new event report cache in Mongo collection when collection is empty" in {
      val data = Json.parse("""{"data":"1"}""")
      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(pstr1, edi, data)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry](filters).toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 1
      }
    }

    "update an existing event report cache in Mongo collection" in {
      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(pstr1, edi, data1)
        _ <- eventReportCacheRepository.upsert(pstr1, edi, data2)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry](filters).toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 1
          documentsInDB.head.data mustBe data2
      }
    }

    "save a new event report cache in Mongo collection when one of filter is different" in {
      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(pstr1, edi, data1)
        _ <- eventReportCacheRepository.upsert(pstr2, edi, data2)
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
      val filters = Filters.and(Filters.eq("pstr", record._1), Filters.eq("eventType", record._2))

      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(externalId, record._1, record._3)
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
      val filters = Filters.and(Filters.eq("pstr", record1._1), Filters.eq("eventType", record1._2))

      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(externalId, record1._1, record1._3)
        _ <- eventReportCacheRepository.upsert(externalId, record2._1, record2._3)
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
        _ <- eventReportCacheRepository.upsert(externalId, record1._1, record1._3)
        _ <- eventReportCacheRepository.upsert(externalId, record2._1, record2._3)
        _ <- eventReportCacheRepository.upsert(externalId + "other", record1._1, record1._3)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry]().toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 3
      }
    }
  }


  "changeVersion" must {
    "update version when present" in {
      val edi2 = EventDataIdentifier(EventType.Event22, 2020, 2, externalId)
      val filters2 = searchFilter(pstr1, edi2)

      val data = Json.parse("""{"data":"1"}""")
      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(pstr1, edi, data)
        result <- eventReportCacheRepository.changeVersion(pstr1, edi, 2)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry](filters).toFuture()
        documentsInDB2 <- eventReportCacheRepository.collection.find[EventReportCacheEntry](filters2).toFuture()
      } yield Tuple3(documentsInDB, documentsInDB2, result)

      whenReady(documentsInDB) {
        case (documentsInDB, documentsInDB2, result) =>
          documentsInDB.size mustBe 0
          documentsInDB2.size mustBe 1
          result.header.status mustBe NO_CONTENT
      }
    }

    "not insert if original not present" in {
      val edi2 = EventDataIdentifier(EventType.Event22, 2020, 2, externalId)
      val filters2 = searchFilter(pstr1, edi2)
      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        result <- eventReportCacheRepository.changeVersion(pstr1, edi, 2)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry](filters2).toFuture()
      } yield Tuple2(documentsInDB, result)

      whenReady(documentsInDB) {
        case (documentsInDB, result) =>
          documentsInDB.size mustBe 0
          result.header.status mustBe NOT_FOUND
      }
    }
  }

  "removeAllButVersion" must {
    "remove all records except version" in {
      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(pstr1, edi, data1)
        _ <- eventReportCacheRepository.upsert(pstr3, edi2, data3)
        _ <- eventReportCacheRepository.upsert(pstr2, edi3, data2)
        _ <- eventReportCacheRepository.removeAllButVersion(externalId, 2)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry]().toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) { documentsInDB =>
        documentsInDB.size mustBe 2
        val doc1 = documentsInDB.head
        val doc2 = documentsInDB(1)
        doc1.edi.version mustBe 2
        doc2.edi.version mustBe 3
      }
    }
  }


  "removeAllOnSignOut" must {
    "remove all records for a given pstr without affecting other data" in {
      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(pstr1, edi, data1)
        _ <- eventReportCacheRepository.upsert(pstr2, edi.copy(externalId = edi.externalId + "other"), data2)
        _ <- eventReportCacheRepository.upsert(pstr3, edi, data3)
        _ <- eventReportCacheRepository.removeAllOnSignOut(externalId)
        documentsInDB <- eventReportCacheRepository.collection.find[EventReportCacheEntry]().toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) { documentsInDB =>
        val doc1 = documentsInDB.head
        documentsInDB.size mustBe 1
        (doc1.pstr, doc1.edi.eventType, doc1.data) mustBe("pstr-2", EventType.Event22, Json.parse("""{"data":"2"}"""))
      }
    }
  }

  "getUserAnswers" must {
    //    "retrieve existing event report cache in Mongo collection when API type specified" in {
    //      val documentsInDB = for {
    //        _ <- eventReportCacheRepository.collection.drop().toFuture()
    //        _ <- eventReportCacheRepository.upsert( pstr1, edi, data1)
    //        documentsInDB <- eventReportCacheRepository.getUserAnswers(externalId, pstr1, Some(edi))
    //      } yield documentsInDB
    //
    //      whenReady(documentsInDB) { documentsInDB =>
    //        documentsInDB.isDefined mustBe true
    //      }
    //    }
    //
    //    "return None when nothing present for API type specified" in {
    //      val documentsInDB = for {
    //        _ <- eventReportCacheRepository.collection.drop().toFuture()
    //        _ <- eventReportCacheRepository.upsert( pstr1, edi, data1)
    //        documentsInDB <- eventReportCacheRepository.getUserAnswers(externalId, pstr1, Some(EventDataIdentifier(Event22, 2020, 1, externalId)))
    //      } yield documentsInDB
    //
    //      whenReady(documentsInDB) { documentsInDB =>
    //        documentsInDB.isDefined mustBe false
    //      }
    //    }

    "retrieve existing event report cache in Mongo collection when NO API type specified" in {
      val record = ("pstr-1", Json.parse("""{"data":"1"}"""))
      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(externalId, record._1, record._2)
        documentsInDB <- eventReportCacheRepository.getUserAnswers(externalId, record._1, None)
      } yield documentsInDB

      whenReady(documentsInDB) { documentsInDB =>
        documentsInDB.isDefined mustBe true
      }
    }

    "return None when nothing present when NO API type specified" in {
      val record = ("pstr-1", Api1826, Json.parse("""{"data":"1"}"""))
      val documentsInDB = for {
        _ <- eventReportCacheRepository.collection.drop().toFuture()
        _ <- eventReportCacheRepository.upsert(pstr1, edi, data1)
        documentsInDB <- eventReportCacheRepository.getUserAnswers(externalId, pstr1, None)
      } yield documentsInDB

      whenReady(documentsInDB) { documentsInDB =>
        documentsInDB.isDefined mustBe false
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

