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

import com.mongodb.client.model.FindOneAndUpdateOptions
import models.ToggleDetails
import org.mockito.Mockito._
import org.mongodb.scala.model.Updates.set
import org.mongodb.scala.model.{Filters, Updates}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatestplus.mockito.MockitoSugar
import play.api.Configuration
import play.api.libs.json.{JsObject, Json}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.Codecs

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class ToggleDataRepositorySpec extends AnyWordSpec with MockitoSugar with Matchers with EmbeddedMongoDBSupport with BeforeAndAfter with
  BeforeAndAfterEach with BeforeAndAfterAll with ScalaFutures { // scalastyle:off magic.number

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds), Span(1, Millis))

  import ToggleDataRepositorySpec._

  var toggleDataRepository: ToggleDataRepository = _

  private val toggleDetails = ToggleDetails("Test-feature-toggle", Some("Test description"), isEnabled = true)

  override def beforeAll(): Unit = {
    when(mockAppConfig.get[String](path = "mongodb.toggle-data.name")).thenReturn("toggle-data")
    initMongoDExecutable()
    startMongoD()
    toggleDataRepository = buildFormRepository(mongoHost, mongoPort)
    super.beforeAll()
  }

  override def afterAll(): Unit =
    stopMongoD()

  private def upsertJsObject(jsonObjectToInsert: JsObject, toggleName: String): Future[Unit] = {
    toggleDataRepository.collection.findOneAndUpdate(
      filter = Filters.eq("toggleName", toggleName),
      update = Updates.combine(
        set("toggleName", toggleName),
        set("toggleDescription", "Test description"),
        set("data", Codecs.toBson(jsonObjectToInsert))
      ), new FindOneAndUpdateOptions().upsert(true)
    ).toFuture().map(_ => ())
  }

  "getAllFeatureToggles" must {
    "get FeatureToggles from Mongo collection" in {
      val documentsInDB = for {
        _ <- toggleDataRepository.collection.drop().toFuture()
        _ <- upsertJsObject(Json.obj(
          "toggleName" -> "Test-toggle",
          "toggleDescription" -> "Test description",
          "isEnabled" -> true
        ), "Test-toggle")
        _ <- upsertJsObject(Json.obj(
          "toggleName" -> "Test-toggle 2",
          "toggleDescription" -> "Test description 2",
          "isEnabled" -> true
        ), "Test-toggle 2")
        documentsInDB <- toggleDataRepository.getAllFeatureToggles
      } yield documentsInDB

      whenReady(documentsInDB) { documentsInDB =>
        documentsInDB.size mustBe 2
      }
    }
  }

  "upsertFeatureToggle" must {
    "create a new FeatureToggle in the Mongo collection" in {
      val documentsInDB = for {
        _ <- toggleDataRepository.collection.drop().toFuture()
        _ <- toggleDataRepository.upsertFeatureToggle(toggleDetails)
        documentsInDB <- toggleDataRepository.collection.find().toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) { documentsInDB =>
        documentsInDB.size mustBe 1
      }
    }
  }

  "deleteFeatureToggle" must {
    "delete a feature toggle in the Mongo collection" in {

      val documentsInDB = for {
        _ <- toggleDataRepository.collection.drop().toFuture()
        _ <- upsertJsObject(Json.obj(
          "toggleName" -> "Test-toggle 1",
          "toggleDescription" -> "Test description 1",
          "isEnabled" -> true
        ), "Test-toggle 1")
        _ <- upsertJsObject(Json.obj(
          "toggleName" -> "Test-toggle 2",
          "toggleDescription" -> "Test description 2",
          "isEnabled" -> false
        ), "Test-toggle 2")
        _ <- toggleDataRepository.deleteFeatureToggle("Test-toggle 2")
        documentsInDB <- toggleDataRepository.collection.countDocuments().toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) { documentsInDB =>
        documentsInDB mustBe 1L
      }
    }
  }
}

object ToggleDataRepositorySpec extends AnyWordSpec with MockitoSugar {

  private val mockAppConfig = mock[Configuration]

  private def buildFormRepository(mongoHost: String, mongoPort: Int) = {
    val databaseName = "pension-scheme-event-reporting"
    val mongoUri = s"mongodb://$mongoHost:$mongoPort/$databaseName?heartbeatFrequencyMS=1000&rm.failover=default"
    new ToggleDataRepository(MongoComponent(mongoUri), mockAppConfig)
  }
}
