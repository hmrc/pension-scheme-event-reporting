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
import models.enumeration.EventType.{Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, WindUp}
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

  def addRecordVersionToUserAnswersJson(eventType: EventType, version: Int, newUserAnswers: JsObject): JsObject = {
    val recordVersionJson: JsObject = {
      val api1826Events: List[EventType] = List(Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, WindUp)
      if (api1826Events.contains(eventType)) {
        Json.obj(s"event${eventType.toString}" -> Json.obj("recordVersion" -> version))
      } else {
        Json.obj()
      }
    }
    newUserAnswers.deepMerge(recordVersionJson)
  }

  private def nodeNameJsObject(eventType: EventType): Option[String] = {
    eventType match {
      case Event11 => Some("event11")
      case Event12 => Some("event12")
      case Event14 => Some("event14")
      case Event18 => Some("event18")
      case WindUp => Some("eventWindUp")
      case _ => None
    }
  }

  private def nodeNameArray(eventType: EventType): Option[String] = {
    eventType match {
      case Event10 => Some("event10")
      case Event13 => Some("event13")
      case Event19 => Some("event19")
      case Event20 => Some("event20")
      case _ => None
    }
  }

  private def getPayloadFromAPIForEventType(futureGetEventResponse: Future[Option[JsObject]],
                                            et: EventType)(implicit ec: ExecutionContext): Future[JsObject] =
    futureGetEventResponse.map {
      case None => Json.obj()
      case Some(payloadFromAPI) =>
        (nodeNameJsObject(et), nodeNameArray(et)) match {
          case (Some(nodeName), None) =>
            (payloadFromAPI \ "eventDetails" \ nodeName).asOpt[JsObject] match {
              case Some(jsonValue) =>
                Json.obj(
                  nodeName -> jsonValue
                )
              case _ => Json.obj()
            }

          case (None, Some(nodeName)) =>
            (payloadFromAPI \ "eventDetails" \ nodeName).asOpt[JsArray] match {
              case Some(jsonValue) =>
                Json.obj(
                  nodeName -> jsonValue
                )
              case _ => Json.obj()
            }

          case _ => Json.obj()
        }
    }

  def collatePayloadsAndUpdateCache(pstr: String, year: Int, currentVersion: String, version: String,
                                    apiType: ApiType, eventTypeForEventBeingCompiled: EventType,
                                    jsonForEventBeingCompiled: JsObject)(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[JsObject] = {
    apiType match {
      case ApiType.Api1826 =>
        lazy val futureGetEventResponse: Future[Option[JsObject]] =
          eventReportConnector.getEvent(pstr, year.toString + "-04-06", currentVersion, None)
        val seqEventTypesToRetrieve = EventType.getEventTypesForAPI(apiType).filter(_ != eventTypeForEventBeingCompiled)
        val versionAsInt = version.toInt
        val transformedPayloads = seqEventTypesToRetrieve.map { et =>
          val gdcdi = GetDetailsCacheDataIdentifier(et, year, versionAsInt)

          getDetailsCacheRepository.get(pstr, gdcdi).flatMap {
            case Some(json) => Future.successful(json.as[JsObject])
            case None =>
              getPayloadFromAPIForEventType(futureGetEventResponse, et)
                .flatMap(payload => getDetailsCacheRepository.upsert(pstr, gdcdi, payload).map(_ => payload))
          }
        }

        val futureJsObject = Future.sequence(transformedPayloads).map { seqPayloads =>
          val eventTypesAsOnePayload = seqPayloads.foldLeft(Json.obj()) { case (acc, payload) => acc ++ payload }
          val originalEventDetails = (jsonForEventBeingCompiled \ EventDetailsNodeName).asOpt[JsObject].getOrElse(Json.obj())
          val originalEventReportDetails = (jsonForEventBeingCompiled \ EventReportDetailsNodeName).asOpt[JsObject].getOrElse(Json.obj())
          Json.obj(EventReportDetailsNodeName -> originalEventReportDetails) ++ Json.obj(
            EventDetailsNodeName -> (eventTypesAsOnePayload ++ originalEventDetails)
          )
        }

        futureJsObject.flatMap { jsObject =>
          getDetailsCacheRepository
            .remove(pstr, GetDetailsCacheDataIdentifier(eventTypeForEventBeingCompiled, year, versionAsInt))
            .map(_ => jsObject)
        }
      case _ => Future.successful(jsonForEventBeingCompiled)
    }
  }
}