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

import org.mockito.Mockito._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}
import org.scalatestplus.mockito.MockitoSugar
import play.api.Configuration
import play.api.libs.json.Json
import uk.gov.hmrc.mongo.MongoComponent

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class CacheRepositorySpec extends AnyWordSpec with MockitoSugar with Matchers with EmbeddedMongoDBSupport with BeforeAndAfter with
  BeforeAndAfterAll with ScalaFutures { // scalastyle:off magic.number

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(Span(30, Seconds), Span(1, Millis))

  import CacheRepositorySpec._

  var cacheRepository: CacheRepository = _

  override def beforeAll(): Unit = {
    initMongoDExecutable()
    startMongoD()
    cacheRepository = buildFormRepository(mongoHost, mongoPort)
    super.beforeAll()
    reset(mockConfiguration)
  }

  override def afterAll(): Unit =
    stopMongoD()


  "remove" must {
    "remove item" in {

      val result = Await.result(
        for {
          _ <- cacheRepository.collection.drop().toFuture()
          _ <- cacheRepository.save(id1, dummyData)
          _ <- cacheRepository.remove(id1)
          status <- cacheRepository.get(id1)
        } yield {
          status
        },
        Duration.Inf
      )
      result mustBe None
    }
  }

  "get" must {
    "return none when nothing present" in {

      val result = Await.result(
        for {
          _ <- cacheRepository.collection.drop().toFuture()
          status <- cacheRepository.get(id1)
        } yield {
          status
        },
        Duration.Inf
      )
      result mustBe None
    }
  }

  "save and get" must {
    "save and get data correctly and have the correct collection name" in {

      val result = Await.result(
        for {
          _ <- cacheRepository.collection.drop().toFuture()
          _ <- cacheRepository.save(id1, dummyData)
          status <- cacheRepository.get(id1)
        } yield {
          status
        },
        Duration.Inf
      )
      result mustBe Some(dummyData)
      cacheRepository.collectionName mustBe collectionName
    }
  }

}

object CacheRepositorySpec extends MockitoSugar {
  private val mockConfiguration = mock[Configuration]

  private val collectionName = "test"

  private val id1 = "id1"

  private val dummyData = Json.obj(
    "test" -> "test"
  )

  private def buildFormRepository(mongoHost: String, mongoPort: Int): CacheRepository = {
    val databaseName = "pension-scheme-accounting-for-tax"
    val mongoUri = s"mongodb://$mongoHost:$mongoPort/$databaseName?heartbeatFrequencyMS=1000&rm.failover=default"
    new CacheRepository(collectionName, Some(60), None, MongoComponent(mongoUri))
  }
}
