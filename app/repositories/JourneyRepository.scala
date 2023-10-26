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

import org.joda.time.DateTime
import org.mongodb.scala.model.{Filters, FindOneAndUpdateOptions, IndexModel, IndexOptions, Indexes, Updates}
import org.mongodb.scala.result.InsertOneResult
import play.api.{Configuration, Logging}
import play.api.libs.json.{Format, JsPath, JsResult, JsValue, Json}
import repositories.JourneyDataEntry._
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}
import uk.gov.hmrc.mongo.play.json.formats.{MongoJavatimeFormats, MongoJodaFormats}
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import java.nio.charset.StandardCharsets
import java.time.{LocalDateTime, ZoneId}
import java.util.{Base64, UUID}
import java.util.concurrent.TimeUnit
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

case class JourneyInsertData(
                             pstr: String,
                             schemeName: String,
                             returnUrl: String,
                             externalId: String,
                             psaPspId: String
                           )
case class JourneyDataEntry(journeyId: String,
                            pstr: String,
                            schemeName: String,
                            returnUrl: String,
                            externalId: String,
                            psaPspId: String,
                            lastUpdated: LocalDateTime,
                            expireAt: LocalDateTime)

object JourneyDataEntry {
  implicit val formats: Format[JourneyDataEntry] = new Format[JourneyDataEntry] {
    override def writes(o: JourneyDataEntry): JsValue = Json.writes[JourneyDataEntry].writes(o)

    private val localDateTimeReads = MongoJavatimeFormats.instantReads.map(LocalDateTime.ofInstant(_, ZoneId.of("UTC")))

    override def reads(json: JsValue): JsResult[JourneyDataEntry] = {
      (
        (JsPath \ journeyIdKey).read[String] and
        (JsPath \ pstrKey).read[String] and
          (JsPath \ schemeNameKey).read[String] and
          (JsPath \ returnUrlKey).read[String] and
          (JsPath \ externalIdKey).read[String] and
            (JsPath \ psaPspIdKey).read[String] and
          (JsPath \ lastUpdatedKey).read(localDateTimeReads) and
          (JsPath \ expireAtKey).read(localDateTimeReads)
        )(
        (journeyId, pstr, schemeName, returnUrl, externalId, psaOrPspId, lastUpdated, expireAt) =>
          JourneyDataEntry(journeyId, pstr, schemeName, returnUrl, externalId, psaOrPspId, lastUpdated, expireAt)
      ).reads(json)
    }
  }

  implicit val dateFormat: Format[DateTime] = MongoJodaFormats.dateTimeFormat
  val journeyIdKey = "journeyId"
  val pstrKey = "pstr"
  val schemeNameKey = "schemeName"
  val returnUrlKey = "returnUrl"
  val externalIdKey = "externalId"
  val psaPspIdKey = "psaPspId"
  val lastUpdatedKey = "lastUpdated"
  val expireAtKey = "expireAt"

  val indexes = Seq(journeyIdKey, pstrKey, psaPspIdKey, externalIdKey)

}
class JourneyRepository @Inject()(
                                 mongoComponent: MongoComponent,
                                 config: Configuration
                                 )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[JourneyDataEntry](
    collectionName = config.underlying.getString("mongodb.journey-data.name"),
    mongoComponent = mongoComponent,
    domainFormat = JourneyDataEntry.formats,
    indexes = Seq(
      IndexModel(
        Indexes.ascending(expireAtKey),
        IndexOptions().name("dataExpiry").expireAfter(0, TimeUnit.SECONDS).background(true)
      ),
      IndexModel(
        Indexes.ascending(indexes:_*),
        IndexOptions().name(indexes.fold("")(_ + "_" + _) + "_idx").background(true).unique(true)
      )
    ),
    extraCodecs = Seq(
      Codecs.playFormatCodec(formats),
      Codecs.playFormatCodec(MongoJavatimeFormats.instantFormat)
    )
  ) with Logging {

  private val expireInSeconds = config.get[Int](path = "mongodb.journey-data.timeToLiveInSeconds")

  private def evaluatedExpireAt: LocalDateTime = {
    LocalDateTime.now(ZoneId.of("UTC")).plusSeconds(expireInSeconds)
  }
  def insert(journeyInsertData: JourneyInsertData): Future[(InsertOneResult, String)] = {
    val journeyId = Base64.getEncoder.encodeToString(UUID.randomUUID().toString.getBytes(StandardCharsets.UTF_8))
    val journeyDataEntry = JourneyDataEntry(
      journeyId,
      journeyInsertData.pstr,
      journeyInsertData.schemeName,
      journeyInsertData.returnUrl,
      journeyInsertData.externalId,
      journeyInsertData.psaPspId,
      LocalDateTime.now(ZoneId.of("UTC")),
      evaluatedExpireAt
    )
    collection.insertOne(journeyDataEntry).toFuture().map(_ -> journeyId)
  }

  def upsert(journeyInsertData: JourneyInsertData): Future[String] = {
    val journeyId = Base64.getEncoder.encodeToString(UUID.randomUUID().toString.getBytes(StandardCharsets.UTF_8))
    val modifier = Updates.combine(
      Updates.set(journeyIdKey, journeyId),
      Updates.set(pstrKey, journeyInsertData.pstr),
      Updates.set(schemeNameKey, journeyInsertData.schemeName),
      Updates.set(returnUrlKey, journeyInsertData.returnUrl),
      Updates.set(externalIdKey, journeyInsertData.externalId),
      Updates.set(psaPspIdKey, journeyInsertData.psaPspId),
      Updates.set(lastUpdatedKey, LocalDateTime.now(ZoneId.of("UTC"))),
      Updates.set(expireAtKey, evaluatedExpireAt)
    )

    val selector = Filters.and(
      Filters.equal(journeyIdKey, journeyId)
    )

    collection.findOneAndUpdate(
      filter = selector,
      update = modifier, new FindOneAndUpdateOptions().upsert(true)).toFuture().map( _ => journeyId)
  }

  def get(journeyId:String, externalId:String, psaPspId:String): Future[JourneyDataEntry] = {
    collection.findOneAndUpdate(
      filter = Filters.and(
        Filters.equal(journeyIdKey, journeyId),
        Filters.equal(externalIdKey, externalId),
        Filters.equal(psaPspIdKey, psaPspId)
      ),
      update = Updates.combine(
        Updates.set(lastUpdatedKey, LocalDateTime.now(ZoneId.of("UTC"))),
        Updates.set(expireAtKey, evaluatedExpireAt)
      ),
      new FindOneAndUpdateOptions()
    ).toFuture()
  }
}
