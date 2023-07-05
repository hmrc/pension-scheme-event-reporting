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

package services

import com.google.inject.{Inject, Singleton}
import connectors.EventReportConnector
import models.GetDetailsCacheDataIdentifier
import models.enumeration.{ApiType, EventType}
import play.api.Logging
import play.api.libs.json._
import repositories.GetDetailsCacheRepository
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class CompilePayloadService @Inject()(
                                       getDetailsCacheRepository: GetDetailsCacheRepository,
                                       eventReportConnector: EventReportConnector
                                     ) extends Logging {

  private final val EventReportDetailsNodeName = "eventReportDetails"
  private final val EventDetailsNodeName = "eventDetails"

  // TODO: Refactor & tidy up code below (use for yields probably for clarity)
  // scalastyle:off method.length
  def interpolateJsonIntoFullPayload(pstr: String,
                                     year: Int,
                                     version: Int,
                                     apiType: ApiType,
                                     eventTypeForEventBeingCompiled: EventType,
                                     jsonForEventBeingCompiled: JsObject)(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[JsObject] = {
    EventType.getEventTypesForAPI(apiType) match {
      case seqEventTypes if seqEventTypes.nonEmpty =>
        val transformedPayloads: Seq[Future[JsObject]] = seqEventTypes.filter(_ != eventTypeForEventBeingCompiled).map { et =>
          val gdcdi = GetDetailsCacheDataIdentifier(et, year, version)
          getDetailsCacheRepository.get(pstr, gdcdi).flatMap {
            case None =>
              val startDate = year.toString + "-04-06"
              eventReportConnector.getEvent(pstr, startDate, version, Some(et)).flatMap {
                case Some(responsePayload) =>
                  getDetailsCacheRepository.upsert(pstr, gdcdi, responsePayload)
                    .map { _ => responsePayload }
                case None =>
                  val responsePayload = Json.obj()
                  getDetailsCacheRepository.upsert(pstr, gdcdi, responsePayload)
                    .map { _ => responsePayload }
                  Future.successful(responsePayload)
              }
            case Some(json) => Future.successful(json.as[JsObject])
          }
        }

        val futureJsObject = Future.sequence(transformedPayloads).map { seqPayloads =>
          val allEventTypesAsOnePayload = seqPayloads.foldLeft(Json.obj()) { case (acc, payload) =>
            val eventDetailsNode = (payload \ EventDetailsNodeName).asOpt[JsObject].getOrElse(Json.obj())
            acc ++ eventDetailsNode
          }
          val originalEventDetails = (jsonForEventBeingCompiled \ EventDetailsNodeName).asOpt[JsObject].getOrElse(Json.obj())
          val originalEventReportDetails = (jsonForEventBeingCompiled \ EventReportDetailsNodeName).asOpt[JsObject].getOrElse(Json.obj())
          Json.obj(EventReportDetailsNodeName -> originalEventReportDetails) ++ Json.obj(
            EventDetailsNodeName -> (originalEventDetails ++ allEventTypesAsOnePayload)
          )
        }

        futureJsObject.flatMap{ jsObject =>
          val gdcdi = GetDetailsCacheDataIdentifier(eventTypeForEventBeingCompiled, year, version)
          getDetailsCacheRepository.remove(pstr, gdcdi).map { _ =>
            jsObject
          }
        }
      case _ => Future.successful(jsonForEventBeingCompiled)
    }
  }
}