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

import connectors.EventReportConnector
import models.GetDetailsCacheDataIdentifier
import models.enumeration.EventType.{Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, WindUp}
import models.enumeration.{ApiType, EventType}
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalacheck.Shrink
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.libs.json._
import repositories.GetDetailsCacheRepository
import uk.gov.hmrc.http.HeaderCarrier
import utils.{GeneratorAPI1826, GeneratorAPI1834, JSONSchemaValidator, JsonFileReader}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Success

class CompilePayloadServiceSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfterEach
  with JsonFileReader with GeneratorAPI1834 with GeneratorAPI1826 with ScalaFutures {

  private def validator = new JSONSchemaValidator

  private val mockGetDetailsCacheRepository = mock[GetDetailsCacheRepository]
  private val mockEventReportConnector = mock[EventReportConnector]
  private val pstr = "pstr"
  private val year = 2022
  private val version = "1"
  private val startDate = s"$year-04-06"
  private val eventTypesFor1834ExcludingEvent11: Seq[EventType] = Seq(WindUp, Event10, Event18, Event13, Event20, Event12, Event14, Event19)
  private val eventTypesFor1834ExcludingEvent10: Seq[EventType] = Seq(WindUp, Event18, Event13, Event20, Event12, Event14, Event19, Event11)
  private val eventTypesFor1834ExcludingEvent10And11: Seq[EventType] = Seq(WindUp, Event18, Event13, Event20, Event12, Event14, Event19)
  private implicit val hc: HeaderCarrier = HeaderCarrier()

  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny // Stop scalacheck from auto-shrinking:-

  private final val SchemaPath1826 = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"

  private def ifResponsesByEventType(eventTypes: Seq[EventType]): Map[EventType, JsObject] = {
    eventTypes.foldLeft[Map[EventType, JsObject]](Map.empty)((acc, et) =>
      acc ++ Map(et ->
        (generateUserAnswersAndPOSTBodyByEvent(et).sample.get._1 \ "eventDetails").as[JsObject]
      )
    )
  }

  private def ifResponseFromGetEvent(eventTypes: Seq[EventType]): JsObject = {
    Json.obj(
      "eventDetails" -> ifResponsesByEventType(eventTypes).foldLeft(Json.obj()) { case (acc, Tuple2(_, jsObject)) =>
        acc ++ jsObject
      }
    )
  }

  override def beforeEach(): Unit = {
    reset(mockGetDetailsCacheRepository)
    reset(mockEventReportConnector)
  }


  "collatePayloadsAndUpdateCache" must {
    "interpolate event 11 payload to all other event types for 1834 (summary) where all event types except event 11 are in cache" in {
      val event11Payload = generateUserAnswersAndPOSTBodyEvent11.sample.get._2
      val eventTypes = ifResponsesByEventType(eventTypesFor1834ExcludingEvent11)
      eventTypesFor1834ExcludingEvent11.foreach { et =>
        val gdcdi = GetDetailsCacheDataIdentifier(et, year, version.toInt)
        val etmpResponse = eventTypes(et)
        when(mockGetDetailsCacheRepository.get(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi))(any()))
          .thenReturn(Future.successful(Some(etmpResponse)))
      }
      when(mockGetDetailsCacheRepository
        .remove(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(GetDetailsCacheDataIdentifier(Event11, year, version.toInt)))(any()))
        .thenReturn(Future.successful((): Unit))

      val service = new CompilePayloadService(mockGetDetailsCacheRepository, mockEventReportConnector)
      whenReady(service.collatePayloadsAndUpdateCache(pstr, year, version, ApiType.Api1826,
        EventType.Event11, event11Payload)(global, implicitly)) { result =>
        validator.validatePayload(result, SchemaPath1826, "API1834") mustBe Success((): Unit)
        val eventDetailsNode = (result \ "eventDetails").as[JsObject]
        verify(mockGetDetailsCacheRepository, times(1))
          .remove(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(GetDetailsCacheDataIdentifier(Event11, year, version.toInt)))(any())

        val nodes = eventDetailsNode.fields.map(_._1).toSet
        nodes mustBe Set("event11", "event10", "event12", "event13", "event14", "event18", "event19", "event20", "eventWindUp")

        val event11ToBeCompiled = (event11Payload \ "eventDetails" \ "event11").as[JsObject]
        val finalResultEvent11 = (eventDetailsNode \ "event11").as[JsObject]
        finalResultEvent11 mustBe event11ToBeCompiled

      }
    }


    "interpolate event 11 payload to other event types where all event types including event 11 but excluding event 10 are in cache" in {
      val allEvents = eventTypesFor1834ExcludingEvent10
      val payloadsByEventType = ifResponsesByEventType(allEvents)
      val event11Payload = generateUserAnswersAndPOSTBodyEvent11.sample.get._2
      allEvents.foreach { et =>
        val gdcdi = GetDetailsCacheDataIdentifier(et, year, version.toInt)
        val etmpResponse = payloadsByEventType(et)
        when(mockGetDetailsCacheRepository.get(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi))(any()))
          .thenReturn(Future.successful(Some(etmpResponse)))
      }
      when(mockGetDetailsCacheRepository.get(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(GetDetailsCacheDataIdentifier(Event10, year, version.toInt)))(any()))
        .thenReturn(Future.successful(Some(Json.obj())))
      when(mockGetDetailsCacheRepository
        .remove(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(GetDetailsCacheDataIdentifier(Event11, year, version.toInt)))(any()))
        .thenReturn(Future.successful((): Unit))

      val service = new CompilePayloadService(mockGetDetailsCacheRepository, mockEventReportConnector)
      whenReady(service.collatePayloadsAndUpdateCache(pstr, year, version, ApiType.Api1826,
        EventType.Event11, event11Payload)(global, implicitly)) { result =>

        validator.validatePayload(result, SchemaPath1826, "API1834") mustBe Success((): Unit)
        val eventDetailsNode = (result \ "eventDetails").as[JsObject]
        verify(mockGetDetailsCacheRepository, times(1))
          .remove(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(GetDetailsCacheDataIdentifier(Event11, year, version.toInt)))(any())

        val event11ToBeCompiled = (event11Payload \ "eventDetails" \ "event11").as[JsObject]
        val finalResultEvent11 = (eventDetailsNode \ "event11").as[JsObject]
        finalResultEvent11 mustBe event11ToBeCompiled

        val nodes = eventDetailsNode.fields.map(_._1).toSet
        nodes mustBe Set("event11", "event12", "event13", "eventWindUp", "event19", "event18", "event14", "event20")
      }
    }

    "interpolate event 11 payload to all other event types for 1834 (summary) where nothing in cache but values present in API but not for events 10 & 11" in {
      when(mockEventReportConnector.getEvent(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(startDate),
        ArgumentMatchers.eq(version), ArgumentMatchers.eq(None))(any(), any()))
        .thenReturn(Future.successful(Some(ifResponseFromGetEvent(eventTypesFor1834ExcludingEvent10And11))))

      val event11Payload = generateUserAnswersAndPOSTBodyEvent11.sample.get._2
      eventTypesFor1834ExcludingEvent11.foreach { et =>
        val gdcdi = GetDetailsCacheDataIdentifier(et, year, version.toInt)
        when(mockGetDetailsCacheRepository.get(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi))(any()))
          .thenReturn(Future.successful(None))
        when(mockGetDetailsCacheRepository.upsert(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi), any())(any()))
          .thenReturn(Future.successful((): Unit))
      }

      val gdcdi = GetDetailsCacheDataIdentifier(Event11, year, version.toInt)
      when(mockGetDetailsCacheRepository.remove(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi))(any()))
        .thenReturn(Future.successful((): Unit))

      val service = new CompilePayloadService(mockGetDetailsCacheRepository, mockEventReportConnector)
      whenReady(service.collatePayloadsAndUpdateCache(pstr, year, version, ApiType.Api1826,
        EventType.Event11, event11Payload)(global, implicitly)) { result =>
        validator.validatePayload(result, SchemaPath1826, "API1834") mustBe Success((): Unit)
        val eventDetailsNode = (result \ "eventDetails").as[JsObject]
        eventTypesFor1834ExcludingEvent11.foreach { et =>
          val gdcdi = GetDetailsCacheDataIdentifier(et, year, version.toInt)
          verify(mockGetDetailsCacheRepository, times(1))
            .upsert(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi), any())(any())
        }
        verify(mockGetDetailsCacheRepository, times(1))
          .remove(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(GetDetailsCacheDataIdentifier(Event11, year, version.toInt)))(any())
        val nodes = eventDetailsNode.fields.map(_._1).toSet
        nodes mustBe Set("event11", "event12", "event13", "eventWindUp", "event19", "event18", "event14", "event20")
      }
    }


    "interpolate event 11 payload to all other event types for 1834 (summary) where nothing in cache or in API" in {
      val event11Payload = generateUserAnswersAndPOSTBodyEvent11.sample.get._2
      eventTypesFor1834ExcludingEvent11.foreach { et =>
        val gdcdi = GetDetailsCacheDataIdentifier(et, year, version.toInt)
        when(mockGetDetailsCacheRepository.get(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi))(any()))
          .thenReturn(Future.successful(None))
        when(mockGetDetailsCacheRepository.upsert(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi), any())(any()))
          .thenReturn(Future.successful((): Unit))
        when(mockEventReportConnector.getEvent(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(startDate),
          ArgumentMatchers.eq(version), ArgumentMatchers.eq(None))(any(), any()))
          .thenReturn(Future.successful(None))
      }
      val gdcdi = GetDetailsCacheDataIdentifier(Event11, year, version.toInt)
      when(mockGetDetailsCacheRepository.remove(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi))(any()))
        .thenReturn(Future.successful((): Unit))

      val service = new CompilePayloadService(mockGetDetailsCacheRepository, mockEventReportConnector)
      whenReady(service.collatePayloadsAndUpdateCache(pstr, year, version, ApiType.Api1826,
        EventType.Event11, event11Payload)(global, implicitly)) { result =>
        validator.validatePayload(result, SchemaPath1826, "API1834") mustBe Success((): Unit)
        val eventDetailsNode = (result \ "eventDetails").as[JsObject]
        eventTypesFor1834ExcludingEvent11.foreach { et =>
          val gdcdi = GetDetailsCacheDataIdentifier(et, year, version.toInt)
          verify(mockGetDetailsCacheRepository, times(1))
            .upsert(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(gdcdi), any())(any())
        }
        verify(mockGetDetailsCacheRepository, times(1))
          .remove(ArgumentMatchers.eq(pstr), ArgumentMatchers.eq(GetDetailsCacheDataIdentifier(Event11, year, version.toInt)))(any())
        val nodes = eventDetailsNode.fields.map(_._1)
        nodes mustBe Seq("event11")
      }
    }

  }

}