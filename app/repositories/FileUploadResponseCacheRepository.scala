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

import com.google.inject.{Inject, Singleton}
import com.mongodb.client.model.FindOneAndUpdateOptions
import org.mongodb.scala.model._
import play.api.libs.json._
import play.api.{Configuration, Logging}
import repositories.FileUploadResponseCacheEntry.{apiTypesKey, expireAtKey, referenceKey}
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.formats.MongoJavatimeFormats
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.{ExecutionContext, Future}


case class FileUploadResponseCacheEntry(pstr: String, apiTypes: String, data: JsValue, lastUpdated: Instant, expireAt: Instant)

object FileUploadResponseCacheEntry {

  implicit val dateFormat: Format[Instant] = MongoJavatimeFormats.instantFormat
  implicit val format: Format[FileUploadResponseCacheEntry] = Json.format[FileUploadResponseCacheEntry]

  val referenceKey = "reference"
  val apiTypesKey = "apiTypes"
  val expireAtKey = "expireAt"
  val lastUpdatedKey = "lastUpdated"
  val dataKey = "data"
}

@Singleton
class FileUploadResponseCacheRepository @Inject()(
                                            mongoComponent: MongoComponent,
                                            config: Configuration
                                          )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[JsValue](
    collectionName = config.underlying.getString("mongodb.file-upload-response.name"),
    mongoComponent = mongoComponent,
    domainFormat = implicitly[Format[JsValue]],
    indexes = Seq(
      IndexModel(
        Indexes.ascending(expireAtKey),
        IndexOptions().name("dataExpiry").expireAfter(0, TimeUnit.SECONDS).background(true)
      ),
      IndexModel(
        Indexes.ascending(referenceKey, apiTypesKey),
        IndexOptions().name(referenceKey).background(true)
      )
    )
  ) with Logging {

  import FileUploadResponseCacheEntry._

  private val expireInSeconds = config.get[Int](path = "mongodb.file-upload-response.timeToLiveInSeconds")

  private def evaluatedExpireAt: Instant = Instant.now().plusSeconds(expireInSeconds + 1)

  private def selector(reference: String) = Filters.equal(referenceKey, reference)

  def upsert(reference: String, data: JsValue)(implicit ec: ExecutionContext): Future[Unit] = {
    val lastUpdated = Instant.now()
    val modifier = Updates.combine(
      Updates.set(referenceKey, Codecs.toBson(reference)),
      Updates.set(dataKey, Codecs.toBson(Json.toJson(data))),
      Updates.set(lastUpdatedKey, Codecs.toBson(lastUpdated)),
      Updates.set(expireAtKey, Codecs.toBson(evaluatedExpireAt))
    )


    collection.findOneAndUpdate(
      filter = selector(reference),
      update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map(_ => ())
  }

  def get(reference: String)(implicit ec: ExecutionContext): Future[Option[JsValue]] = {
   collection.find(
      filter = selector(reference)
    ).headOption().map{ a => a.flatMap{ jsValue =>
     (jsValue.as[JsObject] \ "data").asOpt[JsValue]
   }}
  }
}
