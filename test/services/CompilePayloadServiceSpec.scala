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
import models.enumeration.EventType.{Event10, Event12, Event13, Event14, Event18, Event19, Event20, WindUp}
import models.enumeration.{ApiType, EventType}
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.libs.json._
import repositories.GetDetailsCacheRepository
import utils.{GeneratorAPI1826, GeneratorAPI1834, JSONSchemaValidator, JsonFileReader}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success

class CompilePayloadServiceSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfterEach
  with JsonFileReader with GeneratorAPI1834 with GeneratorAPI1826 with ScalaFutures {

  private def validator = new JSONSchemaValidator

  private val mockGetDetailsCacheRepository = mock[GetDetailsCacheRepository]
  private val pstr = "pstr"
  private val year = 2022
  private val version = 1
  private val eventTypesFor1834 = Seq(WindUp, Event10, Event18, Event13, Event20, Event12, Event14, Event19)
  private val json = Json.obj(

  )

  private final val SchemaPath1826 = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"

  override def beforeEach(): Unit = {
    reset(mockGetDetailsCacheRepository)
  }


  "interpolateJsonIntoFullPayload" must {
    "add in all other event types for 1834 (summary) to event 11 for compile" in {
      val payloadsByEventType = eventTypesFor1834.foldLeft[Map[EventType, JsObject]](Map.empty)((acc, et) =>
        acc ++ Map(et -> generateUserAnswersAndPOSTBodyByEvent(et).sample.get._1)
      )
      val event11Payload = generateUserAnswersAndPOSTBodyEvent11.sample.get._2
      eventTypesFor1834.foreach { et =>
        val gdcdi = GetDetailsCacheDataIdentifier(et, year, version)
        val etmpResponse = payloadsByEventType(et)
        when(mockGetDetailsCacheRepository.get(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi))(any()))
          .thenReturn(Future.successful(Some(etmpResponse)))
      }

      val service = new CompilePayloadService(mockGetDetailsCacheRepository)
      whenReady(service.interpolateJsonIntoFullPayload(pstr, year, version, ApiType.Api1834,
        EventType.Event11, event11Payload)(global)) { result =>
//        println("\n>>>RES" + result)
        validator.validatePayload(result, SchemaPath1826, "API1834") mustBe Success((): Unit)
        val eventDetailsNode = (result \ "eventDetails").as[JsObject]
        val nodes = eventDetailsNode.fields.map(_._1)
        nodes mustBe Seq("event11", "eventWindUp", "event10", "event18", "event13", "event20", "event12", "event14", "event19")
      }
    }
  }

}