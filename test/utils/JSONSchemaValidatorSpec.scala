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

package utils

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfter, TryValues}
import org.scalatestplus.mockito.MockitoSugar
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import repositories.{EventReportCacheRepository, OverviewCacheRepository}

class JSONSchemaValidatorSpec extends AnyWordSpec with MockitoSugar with Matchers with BeforeAndAfter with JsonFileReader with TryValues {
  val createCompiledEventSummaryReportSchemaPath = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  val compileEventOneReportSchemaPath = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.1.json"
  val submitEventDeclarationReportSchemaPath = "/resources.schemas/api-1828-submit-event-declaration-report-request-schema-v1.0.0.json"
  val submitEvent20ADeclarationReportSchemaPath = "/resources.schemas/api-1829-submit-event20a-declaration-report-request-schema-v1.0.0.json"
  val compileMemberEventReportSchemaPath = "/resources.schemas/api-1830-create-compiled-member-event-report-request-schema-v1.0.4.json"
  val testEventName = "testEventName"

  private val mockEventReportCacheRepository = mock[EventReportCacheRepository]
  private val mockOverviewCacheRepository = mock[OverviewCacheRepository]

  val modules: Seq[GuiceableModule] =
    Seq(
      bind[EventReportCacheRepository].toInstance(mockEventReportCacheRepository),
      bind[OverviewCacheRepository].toInstance(mockOverviewCacheRepository)
    )

  val app: Application = new GuiceApplicationBuilder()
    .overrides(modules: _*).build()

  private lazy val jsonPayloadSchemaValidator: JSONSchemaValidator = app.injector.instanceOf[JSONSchemaValidator]
  "validateJson" must {
    "Behaviour for valid payload for API 1826" in {
      val json = readJsonFromFile("/api-1826-valid-example.json")
      val result = jsonPayloadSchemaValidator.validatePayload(json, createCompiledEventSummaryReportSchemaPath, testEventName)
      result.success.value mustBe (())
    }

    "Behaviour for invalid payload with 2 invalid inputs for API 1826" in {
      val json = readJsonFromFile("/api-1826-invalid-example.json")
      val result = jsonPayloadSchemaValidator.validatePayload(json, createCompiledEventSummaryReportSchemaPath, testEventName)
      result.failure.exception.getMessage must include(s"Schema validation errors for $testEventName")
      result.failure.exception.getMessage must include("/eventDetails/event10/0/invRegScheme/startDateDetails/startDateOfInvReg")
      result.failure.exception.getMessage must include("/eventDetails/event10/1/invRegScheme/ceaseDateDetails/ceaseDateOfInvReg")
    }

    "Behaviour for valid payload for API 1827" in {
      val json = readJsonFromFile("/api-1827-valid-example.json")
      val result = jsonPayloadSchemaValidator.validatePayload(json, compileEventOneReportSchemaPath, testEventName)
      result.success.value mustBe (())
    }

    "Behaviour for invalid payload with 2 invalid inputs for API 1827" in {
      val json = readJsonFromFile("/api-1827-invalid-example.json")
      val result = jsonPayloadSchemaValidator.validatePayload(json, compileEventOneReportSchemaPath, testEventName)
      result.failure.exception.getMessage must include(s"Schema validation errors for $testEventName")
      result.failure.exception.getMessage must include("/event1Details/event1Details/0/unAuthorisedPaymentDetails/dateOfUnauthorisedPayment")
      result.failure.exception.getMessage must include("/eventReportDetails/reportEndDate")
    }

    "Behaviour for valid payload for API 1828" in {
      val json = readJsonFromFile("/api-1828-valid-example.json")
      val result = jsonPayloadSchemaValidator.validatePayload(json, submitEventDeclarationReportSchemaPath, testEventName)
      result.success.value mustBe (())
    }

    "Behaviour for invalid payload with 2 invalid inputs for API 1828" in {
      val json = readJsonFromFile("/api-1828-invalid-example.json")
      val result = jsonPayloadSchemaValidator.validatePayload(json, submitEventDeclarationReportSchemaPath, testEventName)
      result.failure.exception.getMessage must include(s"Schema validation errors for $testEventName")
      result.failure.exception.getMessage must include("/declarationDetails/erDetails/reportEndDate")
      result.failure.exception.getMessage must include("/declarationDetails/psaDeclaration/psaDeclaration2")
    }

    "Behaviour for valid payload for API 1829" in {
      val json = readJsonFromFile("/api-1829-valid-example.json")
      val result = jsonPayloadSchemaValidator.validatePayload(json, submitEvent20ADeclarationReportSchemaPath, testEventName)
      result.success.value mustBe (())
    }

    "Behaviour for invalid payload with 2 invalid inputs for API 1829" in {
      val json = readJsonFromFile("/api-1829-invalid-example.json")
      val result = jsonPayloadSchemaValidator.validatePayload(json, submitEvent20ADeclarationReportSchemaPath, testEventName)
      result.failure.exception.getMessage must include(s"Schema validation errors for $testEventName")
      result.failure.exception.getMessage must include("/eventReportDetails")
      result.failure.exception.getMessage must include("/eventReportDetails/er20aDetails/reportStartDate")
    }

    "Behaviour for valid payload for API 1830" in {
      val json = readJsonFromFile("/api-1830-valid-example.json")
      val result = jsonPayloadSchemaValidator.validatePayload(json, compileMemberEventReportSchemaPath, testEventName)
      result.success.value mustBe (())
    }

    "Behaviour for invalid payload with 2 invalid inputs for API 1830" in {
      val json = readJsonFromFile("/api-1830-invalid-example.json")
      val result = jsonPayloadSchemaValidator.validatePayload(json, compileMemberEventReportSchemaPath, testEventName)
      result.failure.exception.getMessage must include(s"Schema validation errors for $testEventName")
      result.failure.exception.getMessage must include("/memberEventsDetails/eventDetails/0/memberDetail/event")
      result.failure.exception.getMessage must include("/memberEventsDetails/eventReportDetails/reportEndDate")
    }
  }
}