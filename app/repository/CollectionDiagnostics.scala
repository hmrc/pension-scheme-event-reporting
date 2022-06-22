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

package repository

import org.slf4j.{Logger, LoggerFactory}
import reactivemongo.api.ReadConcern
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class IndexDef(name: String, fields: Seq[String], unique: Boolean, ttl: Option[Int])

object CollectionDiagnostics {

  val logger: Logger = LoggerFactory.getLogger("CollectionDiagnostics")

  def logCollectionInfo(collection: JSONCollection): Unit = {

    indexInfo(collection) map {
      indexes =>
        logger.warn(
          s"Diagnostic information for collection ${collection.name}\n\n" +
            s"Index definitions\n\n" +
            (indexes.map {
              index =>
                s"Name:   ${index.name}\n" +
                  s"Fields: ${index.fields.mkString(", ")}\n" +
                  s"Unique: ${index.unique}\n" +
                  s"TTL:    ${index.ttl.getOrElse("<none>")}\n"
            } mkString "\n")
        )
    }

    collection.count(None, None, skip = 0, None, ReadConcern.Local).foreach { count =>
      logger.warn(s"\nRow count for collection ${collection.name} : $count\n\n")
    }
  }

  def indexInfo(collection: JSONCollection): Future[Seq[IndexDef]] = {

    collection.indexesManager.list().map {
      indexes =>
        indexes.map {
          index =>
            val ttl = index.options.getAs[Int]("expireAfterSeconds")
            IndexDef(
              index.eventualName,
              index.key.map(_._1),
              index.unique,
              ttl
            )
        }
    }
  }
}
