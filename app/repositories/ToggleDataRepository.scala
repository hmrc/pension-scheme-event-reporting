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

import com.google.inject.Inject
import com.mongodb.client.model.FindOneAndUpdateOptions
import models.ToggleDetails
import org.mongodb.scala.model.Updates.set
import org.mongodb.scala.model._
import play.api.libs.json._
import play.api.{Configuration, Logging}
import repositories.ToggleDataRepository._
import uk.gov.hmrc.mongo.MongoComponent
import uk.gov.hmrc.mongo.play.json.{Codecs, PlayMongoRepository}

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

object ToggleDataRepository {
  private val toggleNameFieldName = "toggleName"
  private val dataFieldName = "data"
}

@Singleton
class ToggleDataRepository @Inject()(
                                      mongoComponent: MongoComponent,
                                      configuration: Configuration
                                    )(implicit val ec: ExecutionContext)
  extends PlayMongoRepository[JsValue](
    collectionName = configuration.get[String](path = "mongodb.toggle-data.name"),
    mongoComponent = mongoComponent,
    domainFormat = implicitly,
    indexes = Seq(
      IndexModel(
        Indexes.ascending(toggleNameFieldName),
        IndexOptions().name(toggleNameFieldName).unique(true).background(true))
    )
  ) with Logging {

  def upsertFeatureToggle(toggleDetails: ToggleDetails): Future[Unit] = {
    collection.find(
      filter = Filters.eq(toggleNameFieldName, toggleDetails.toggleName)
    ).toFuture().map { seqDbToggles =>
      val dbToggleDesc = seqDbToggles.headOption.flatMap { dbToggle => (dbToggle \ "data" \ "toggleDescription").asOpt[String] }
      val toggleDescription = toggleDetails.toggleDescription match {
        case toggleDesc@Some(_) => toggleDesc
        case None => dbToggleDesc
      }
      val updatedToggleDetails = ToggleDetails(toggleDetails.toggleName, toggleDescription, toggleDetails.isEnabled)
      val seqUpdates = Seq(
        set(toggleNameFieldName, toggleDetails.toggleName),
        set(dataFieldName, Codecs.toBson(updatedToggleDetails))
      )
      val upsertOptions = new FindOneAndUpdateOptions().upsert(true)
      collection.findOneAndUpdate(
        filter = Filters.eq(toggleNameFieldName, toggleDetails.toggleName),
        update = Updates.combine(seqUpdates: _*), upsertOptions).toFuture().map(_ => ())
    }.flatten
  }

  def deleteFeatureToggle(toggleName: String): Future[Unit] = {
    collection.deleteOne(
      filter = Filters.eq(toggleNameFieldName, toggleName)
    ).toFuture().map(_ => ())
  }

  def getAllFeatureToggles: Future[Seq[ToggleDetails]] = {
    collection.find[JsValue]().toFuture().map {
      seqJsValue =>
        seqJsValue map {
          jsVal => (jsVal \ "data").asOpt[ToggleDetails]
        }
    }.map {
      _.flatMap {
        _.toSeq
      }
    }
  }
}
