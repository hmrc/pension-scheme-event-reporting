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


import com.google.inject.Inject
import org.joda.time.{DateTime, DateTimeZone}
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json.{Format, JsObject, JsValue, Json}
import play.modules.reactivemongo.ReactiveMongoComponent
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import uk.gov.hmrc.mongo.ReactiveRepository
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats
import reactivemongo.play.json.ImplicitBSONHandlers._
import scala.concurrent.{ExecutionContext, Future}

class OverviewCacheRepository @Inject()(collectionName: String,
                                        expireInSeconds: Int,
                                        mongoComponent: ReactiveMongoComponent)(implicit val ec: ExecutionContext)
  extends ReactiveRepository[JsValue, BSONObjectID](
    collectionName,
    mongoComponent.mongoConnector.db,
    implicitly
  ) {

  private val collectionIndexes: Seq[Index] = Seq(
    Index(key = Seq(("id", IndexType.Ascending)), name = Some("id"), background = true, unique = true),
    Index(key = Seq(("expireAt", IndexType.Ascending)), name = Some("dataExpiry"), background = true,
      options = BSONDocument("expireAfterSeconds" -> 0))
  )
  override val logger: Logger = LoggerFactory.getLogger("OverviewCacheRepository")

  (for {
    _ <- createIndex(collectionIndexes)
  } yield {
    ()
  }) recoverWith {
    case t: Throwable => Future.successful(logger.error(s"Error creating indexes on collection ${collection.name}", t))
  } andThen {
    case _ => CollectionDiagnostics.logCollectionInfo(collection)
  }

  private def createIndex(indexes: Seq[Index]): Future[Seq[Boolean]] = {
    Future.sequence(
      indexes.map { index =>
        collection.indexesManager.ensure(index) map { result =>
          logger.debug(s"Index $index was created successfully and result is: $result")
          result
        } recover {
          case e: Exception => logger.error(s"Failed to create index $index", e)
            false
        }
      }
    )
  }

  private def cacheExpiry: DateTime = {
    DateTime
      .now(DateTimeZone.UTC)
      .plusSeconds(1800)
  }


  private case class ReportingOverviewCache(id: String, eventDetail: JsValue, lastUpdated: DateTime, expiredAt: DateTime)

  private object ReportingOverviewCache {
    implicit val dateFormat: Format[DateTime] = ReactiveMongoFormats.dateTimeFormats
    implicit val format: Format[ReportingOverviewCache] = Json.format[ReportingOverviewCache]

    def reportingOverviewCache(id: String,
                               eventDetail: JsValue,
                               lastUpdated: DateTime = DateTime.now(DateTimeZone.UTC),
                               expiredAt: DateTime): ReportingOverviewCache = {
      ReportingOverviewCache(id, eventDetail, lastUpdated, expiredAt)
    }
  }

  def save(id: String, eventDetail: JsValue)(implicit ec: ExecutionContext): Future[Boolean] = {
    logger.debug(s"Changes implemented in $collectionName cache")
    val content: JsValue = Json.toJson(ReportingOverviewCache.reportingOverviewCache(
      id = id, eventDetail = eventDetail, lastUpdated = DateTime.now(DateTimeZone.UTC), expiredAt = cacheExpiry))
    val selector = BSONDocument("id" -> id)
    val modifier = BSONDocument("$set" -> content)
    collection.update.one(selector, modifier, upsert = true).map(_.ok)
  }

  def get(id: String)(implicit ec: ExecutionContext): Future[Option[JsValue]] = {
    logger.debug(s"Retrieving data from $collectionName cache")

    collection.find(BSONDocument("id" -> id), projection = Option.empty[JsObject]).one[ReportingOverviewCache].map {
      _.map {
        overviewCache =>
          overviewCache.eventDetail
      }
    }
  }

}
