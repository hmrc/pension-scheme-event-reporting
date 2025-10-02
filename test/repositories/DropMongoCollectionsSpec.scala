/*
 * Copyright 2025 HM Revenue & Customs
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

import org.apache.pekko.util.Timeout
import org.mockito.Mockito.when
import org.mongodb.scala.SingleObservableFuture
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Configuration
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.Helpers.running
import uk.gov.hmrc.mongo.test.CleanMongoCollectionSupport

import scala.concurrent.ExecutionContext.Implicits.global

class DropMongoCollectionsSpec
  extends AnyWordSpec
    with GuiceOneAppPerSuite
    with CleanMongoCollectionSupport
    with Matchers
    with MockitoSugar {

  implicit val timeout: Timeout =
    Timeout(patienceConfig.timeout)

  val mockConfig: Configuration =
    mock[Configuration]

  def appBuilder: GuiceApplicationBuilder =
    new GuiceApplicationBuilder().overrides(
      bind[DropMongoCollections].toInstance(
        new DropMongoCollections(mongoComponent, mockConfig) {
          override lazy val collectionNamesToDrop = Seq("col1", "col2", "not found col")
        }
      )
    )

  "DropMongoCollections" must {
    "drop collections when true returned from config" in {
      when(mockConfig.getOptional[Boolean]("mongodb.drop-unused-collections")).thenReturn(Some(true))

      Seq("col1", "col2", "col3").foreach { collectionName =>
        mongoDatabase.createCollection(collectionName).toFuture().futureValue
      }

      whenReady(mongoDatabase.listCollectionNames().collect().toFuture()) {
        collectionNames =>
          collectionNames.length mustBe 3
          collectionNames.contains("col1") mustBe true
          collectionNames.contains("col2") mustBe true
          collectionNames.contains("col3") mustBe true
      }

      running(appBuilder.build()) {
        whenReady(mongoDatabase.listCollectionNames().collect().toFuture()) {
          collectionNames =>
            collectionNames.length mustBe 1
            collectionNames.contains("col1") mustBe false
            collectionNames.contains("col2") mustBe false
            collectionNames.contains("col3") mustBe true
        }
      }
    }

    Seq(None, Some(false)).foreach { configValue =>
      s"not drop collections when $configValue returned from config" in {
        when(mockConfig.getOptional[Boolean]("mongodb.drop-unused-collections")).thenReturn(configValue)

        Seq("col4", "col5", "col6").foreach { collectionName =>
          mongoDatabase.createCollection(collectionName).toFuture().futureValue
        }

        whenReady(mongoDatabase.listCollectionNames().collect().toFuture()) {
          collectionNames =>
            collectionNames.length mustBe 3
            collectionNames.contains("col4") mustBe true
            collectionNames.contains("col5") mustBe true
            collectionNames.contains("col6") mustBe true
        }

        running(appBuilder.build()) {
          whenReady(mongoDatabase.listCollectionNames().collect().toFuture()) {
            collectionNames =>
              collectionNames.length mustBe 3
              collectionNames.contains("col4") mustBe true
              collectionNames.contains("col5") mustBe true
              collectionNames.contains("col6") mustBe true
          }
        }
      }
    }

  }
}
