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

import com.google.inject.{Inject, Singleton}
import play.api.{Configuration, Logging}
import uk.gov.hmrc.mongo.MongoComponent

import scala.concurrent.ExecutionContext

@Singleton
class DropMongoCollections @Inject()(
  mongoComponent: MongoComponent,
  configuration: Configuration
)(implicit ec: ExecutionContext)
  extends Logging {

  lazy val collectionNamesToDrop: Seq[String] =
    Seq("toggle-data")

  if (configuration.getOptional[Boolean]("mongodb.drop-unused-collections").getOrElse(false)) {
    logger.info("mongodb.drop-unused-collections: true")

    mongoComponent
      .database
      .listCollectionNames()
      .collect()
      .head()
      .map { existingCollectionNames =>
        existingCollectionNames.filter(collectionNamesToDrop.contains)
      }
      .map { filteredCollectionNames =>
        logger.info(s"collections matched from listCollectionNames: ${
          if (filteredCollectionNames.nonEmpty) filteredCollectionNames.mkString(", ") else "0"
        }")

        filteredCollectionNames.foreach {
          collectionName =>
            logger.info(s"dropping $collectionName...")

            mongoComponent
              .database
              .getCollection(collectionName)
              .drop()
              .headOption()
              .map {
                case Some(_) =>
                  logger.info(s"$collectionName dropped")
                case _ =>
                  logger.info(s"$collectionName not dropped")
              }
        }
      }

  } else {
    logger.info("mongodb.drop-unused-collections: false")
  }
}
