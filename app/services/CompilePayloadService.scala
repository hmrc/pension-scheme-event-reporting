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
import models.enumeration.ApiType._
import models.enumeration.EventType._
import models.enumeration.{ApiType, EventType}
import models.{EROverview, ERVersion, EventDataIdentifier, GetDetailsCacheDataIdentifier}
import play.api.Logging
import play.api.http.Status.NOT_IMPLEMENTED
import play.api.libs.json.JsResult.toTry
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc.{RequestHeader, Result}
import repositories.{EventReportCacheRepository, GetDetailsCacheRepository}
import transformations.ETMPToFrontEnd.{API1831, API1832, API1833, API1834}
import transformations.UserAnswersToETMP._
import uk.gov.hmrc.http.{BadRequestException, ExpectationFailedException, HeaderCarrier, HttpResponse}
import utils.JSONSchemaValidator

import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class CompilePayloadService @Inject()(
                                       getDetailsCacheRepository: GetDetailsCacheRepository
                                     ) extends Logging {


  /*
  Remove processingDate & schemeDetails fields
  Add memberEventsDetails wrapper
  Remove eventReportDetails.reportStatus, reportVersionNumber and reportSubmittedDateAndTime
  Add eventReportDetails.pSTR


  memberEventsDetails:
                     eventReportDetails:
                       pSTR: '87219363YN'
                       reportStartDate: '2021-04-06'
                       reportEndDate: '2022-04-05'
                       eventType: 'Event2'
                     eventDetails:
                       - memberDetail:
                           memberStatus: 'New'
                           event:
                             eventType: 'Event2'
                             individualDetails:
                               title: 'Mr'
                               firstName: 'John'
                               middleName: 'A'
                               lastName: 'Smith'
                               nino: 'AA345678B'
                             personReceivedThePayment:
                               title: 'Mr'
                               firstName: 'James'
                               middleName: 'Torner'
                               lastName: 'Mike'
                               nino: 'AA345678A'
                             paymentDetails:
                               amountPaid: 123.01
                               eventDate: '2021-05-30'


   */
  //

  private def transformJson(json: JsObject): JsObject = {
    json
  }

  def interpolateJsonIntoFullPayload(pstr: String, year: Int,
                                     version: Int,
                                     apiType: ApiType,
                                     jsonForEventBeingCompiled: JsObject)(implicit ec: ExecutionContext): Future[JsObject] = {
    EventType.getEventTypesForAPI(apiType) match {
      case seqEventTypes if seqEventTypes.nonEmpty =>
        val transformedPayloads: Seq[Future[JsObject]] = seqEventTypes.map { et =>
          val gdcdi = GetDetailsCacheDataIdentifier(et, year, version)
          getDetailsCacheRepository.get(pstr, gdcdi).map {
            case None =>
              Json.obj()
            case Some(json) =>
              transformJson(json.as[JsObject])
          }
        }
        Future.sequence(transformedPayloads).map { seqPayloads =>
          seqPayloads.foldLeft(jsonForEventBeingCompiled) { case (acc, payload) =>

            val tt = (payload \ "eventDetails").asOpt[JsArray].getOrElse(Json.arr())
            val ggg = Json.obj(
              "memberEventsDetails" -> Json.obj(
                "eventDetails" -> tt
              )
            )
            acc ++ ggg
          }
        }
      case _ => Future.successful(jsonForEventBeingCompiled)
    }
  }
}