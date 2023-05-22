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
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.mongo.MongoComponent

import scala.concurrent.ExecutionContext.Implicits.global

class FileUploadResponseCacheRepositorySpec extends AnyWordSpec with MockitoSugar with Matchers with EmbeddedMongoDBSupport
  with BeforeAndAfter with BeforeAndAfterAll with ScalaFutures { // scalastyle:off magic.number

  import FileUploadResponseCacheRepositorySpec._

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds), Span(1, Millis))

  var fileUploadResponseCacheRepository: FileUploadResponseCacheRepository = _

  override def beforeAll(): Unit = {
    when(mockAppConfig.underlying).thenReturn(mockConfig)
    when(mockConfig.getString("mongodb.file-upload-response.name")).thenReturn("file-upload-response")
    when(mockConfig.getInt("mongodb.file-upload-response.timeToLiveInSeconds")).thenReturn(ttlValue)

    initMongoDExecutable()
    startMongoD()
    fileUploadResponseCacheRepository = buildFromRepository(mongoHost, mongoPort)
  }

  "upsert" must {
    "save a new file upload response in Mongo collection when collection is empty" in {
      val record = ("123", Json.parse("""{"data":"1"}"""))
      val filters = Filters.eq("reference", record._1)

      val documentsInDB = for {
        _ <- fileUploadResponseCacheRepository.collection.drop().toFuture()
        _ <- fileUploadResponseCacheRepository.upsert(record._1, record._2)
        documentsInDB <- fileUploadResponseCacheRepository.collection.find[JsValue](filters).toFuture()
      } yield documentsInDB

      whenReady(documentsInDB) {
        documentsInDB =>
          documentsInDB.size mustBe 1
      }
    }

    "get" must {
      "retrieve existing get-event cache in Mongo collection" in {

        val record = ("123", Json.parse("""{"data":"1"}"""))

        val documentsInDB = for {
          _ <- fileUploadResponseCacheRepository.collection.drop().toFuture()
          _ <- fileUploadResponseCacheRepository.upsert(record._1, record._2)
          documentsInDB <- fileUploadResponseCacheRepository.get(record._1)
        } yield documentsInDB

        whenReady(documentsInDB) { documentsInDB =>
          documentsInDB.isDefined mustBe true
        }
      }
    }
  }
}


object FileUploadResponseCacheRepositorySpec extends AnyWordSpec with MockitoSugar {

  private val mockAppConfig = mock[Configuration]
  private val mockConfig = mock[Config]
  private val ttlValue = 900

  private def buildFromRepository(mongoHost: String, mongoPort: Int) = {
    val databaseName = "pension-scheme-event-reporting"
    val mongoUri = s"mongodb://$mongoHost:$mongoPort/$databaseName?heartbeatFrequencyMS=1000&rm.failover=default"
    new FileUploadResponseCacheRepository(MongoComponent(mongoUri), mockAppConfig)
  }
}



