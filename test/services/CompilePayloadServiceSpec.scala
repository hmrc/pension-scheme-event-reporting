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

import models.GetDetailsCacheDataIdentifier
import models.enumeration.EventType.{Event2, Event22, Event23, Event24, Event3, Event4, Event5, Event6, Event7, Event8, Event8A}
import models.enumeration.{ApiType, EventType}
import org.mockito.ArgumentMatchers
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.libs.json._
import repositories.GetDetailsCacheRepository
import utils.{GeneratorAPI1832, JSONSchemaValidator, JsonFileReader}

import scala.concurrent.Future
import scala.util.Success

class CompilePayloadServiceSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfterEach
  with JsonFileReader with GeneratorAPI1832 {

  private val validator = new JSONSchemaValidator
  private val mockGetDetailsCacheRepository = mock[GetDetailsCacheRepository]
  private val pstr = "pstr"
  private val year = 2022
  private val version = 1
  private val eventTypesFor1832 = Seq(Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A, Event22, Event23, Event24)
  private val json = Json.obj(
    "event" -> "compiling"
  )

  private final val SchemaPath1830 = "/resources.schemas/api-1830-create-compiled-member-event-report-request-schema-v1.0.7.json"

  override def beforeEach(): Unit = {
    reset(mockGetDetailsCacheRepository)
  }

  /*
  Remove processingDate & schemeDetails fields
  Add memberEventsDetails wrapper
  Remove eventReportDetails.reportStatus, reportVersionNumber and reportSubmittedDateAndTime
  Add eventReportDetails.pSTR
   */

  "interpolateJsonIntoFullPayload" must {
    "work for 1832 (member)" in {
      val payloadsByEventType = eventTypesFor1832.foldLeft[Map[EventType, JsObject]](Map.empty) ( (acc, et) =>
        acc ++ Map(et -> generateUserAnswersAndPOSTBodyByEvent(et).sample.get._1)
      )

      eventTypesFor1832.foreach { et =>
        val gdcdi = GetDetailsCacheDataIdentifier(et, year: Int, version: Int)
        val etmpResponse = payloadsByEventType(et)
        when(mockGetDetailsCacheRepository.get(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi)))
          .thenReturn(Future.successful(Some(etmpResponse)))
      }

      val service = new CompilePayloadService(mockGetDetailsCacheRepository)
      val result = service.interpolateJsonIntoFullPayload(ApiType.Api1832, json)

      validator.validatePayload(result, SchemaPath1830, "API1830") mustBe Success(():Unit)
    }
  }

}