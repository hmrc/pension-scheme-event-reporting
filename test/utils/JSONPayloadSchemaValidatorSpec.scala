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

import org.mockito.MockitoSugar
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.inject.guice.GuiceApplicationBuilder

class JSONPayloadSchemaValidatorSpec extends AnyWordSpec with MockitoSugar with Matchers with BeforeAndAfter with JsonFileReader {
  val createCompiledEventSummaryReportSchemaPath = "/resources.schemas/api-1826-create-compiled-event-summary-report-request-schema-v1.0.0.json"
  val compileEventOneReportSchemaPath = "/resources.schemas/api-1827-create-compiled-event-1-report-request-schema-v1.0.1.json"
  val submitEventDeclarationReportSchemaPath = "/resources.schemas/api-1828-submit-event-declaration-report-request-schema-v1.0.0.json"
  val compileMemberEventReportSchemaPath = "/resources.schemas/api-1830-create-compiled-member-event-report-request-schema-v1.0.4.json"

  private val app = new GuiceApplicationBuilder()
    .overrides(
    )
    .build()

  private lazy val jsonPayloadSchemaValidator: JSONPayloadSchemaValidator = app.injector.instanceOf[JSONPayloadSchemaValidator]
  "validateJson" must {
    "Behaviour for valid payload for API 1826" in {
      val json = readJsonFromFile("/api-1826-valid-example.json")
      val result = jsonPayloadSchemaValidator.validateJsonPayload(createCompiledEventSummaryReportSchemaPath, json)
      result.right.get mustBe true
    }

    "Behaviour for invalid payload with 2 invalid inputs for API 1826" in {
      val json = readJsonFromFile("/api-1826-invalid-example.json")
      val result = jsonPayloadSchemaValidator.validateJsonPayload(createCompiledEventSummaryReportSchemaPath, json)
      result.left.get.size mustBe 2
    }

    "Behaviour for valid payload for API 1827" in {
      val json = readJsonFromFile("/api-1827-valid-example.json")
      val result = jsonPayloadSchemaValidator.validateJsonPayload(compileEventOneReportSchemaPath, json)
      result.right.get mustBe true
    }

    "Behaviour for invalid payload with 2 invalid inputs for API 1827" in {
      val json = readJsonFromFile("/api-1827-invalid-example.json")
      val result = jsonPayloadSchemaValidator.validateJsonPayload(compileEventOneReportSchemaPath, json)
      result.left.get.size mustBe 2
    }

    "Behaviour for valid payload for API 1828" in {
      val json = readJsonFromFile("/api-1828-valid-example.json")
      val result = jsonPayloadSchemaValidator.validateJsonPayload(submitEventDeclarationReportSchemaPath, json)
      result.right.get mustBe true
    }

    "Behaviour for invalid payload with 2 invalid inputs for API 1828" in {
      val json = readJsonFromFile("/api-1828-invalid-example.json")
      val result = jsonPayloadSchemaValidator.validateJsonPayload(submitEventDeclarationReportSchemaPath, json)
      result.left.get.size mustBe 2
    }

    "Behaviour for valid payload for API 1830" in {
      val json = readJsonFromFile("/api-1830-valid-example.json")
      val result = jsonPayloadSchemaValidator.validateJsonPayload(compileMemberEventReportSchemaPath, json)
      result.right.get mustBe true
    }

    "Behaviour for invalid payload with 2 invalid inputs for API 1830" in {
      val json = readJsonFromFile("/api-1830-invalid-example.json")
      val result = jsonPayloadSchemaValidator.validateJsonPayload(compileMemberEventReportSchemaPath, json)
      result.left.get.size mustBe 2
    }
  }
}