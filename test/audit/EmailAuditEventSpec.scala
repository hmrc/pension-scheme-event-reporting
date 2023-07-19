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

package audit

import models.Sent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}

class EmailAuditEventSpec extends AnyFlatSpec with Matchers {

  "EmailAuditEvent" should "output the correct map of data for PSA" in {

    val event = EmailAuditEvent(
      psaOrPspId = "A2500001",
      pstr = "pstr-test",
      submittedBy = "PSA",
      emailAddress = "test@test.com",
      event = Sent,
      requestId = "test-request-id",
      reportVersion = "1"
    )

    val expected: JsObject = Json.obj(
      "email-initiation-request-id" -> "test-request-id",
      "PensionSchemeAdministratorId" -> "A2500001",
      "emailAddress" -> "test@test.com",
      "event" -> Sent.toString,
      "submittedBy" -> "PSA",
      "reportVersion" -> "1",
      "PensionSchemeTaxReference" -> "pstr-test"
    )

    event.auditType shouldBe "EventReportingEmailEvent"
    event.details shouldBe expected
  }

  "EmailAuditEvent" should "output the correct map of data for PSP" in {

    val event = EmailAuditEvent(
      psaOrPspId = "A2500001",
      pstr = "pstr-test",
      submittedBy = "PSP",
      emailAddress = "test@test.com",
      event = Sent,
      requestId = "test-request-id",
      reportVersion = "1"
    )

    val expected: JsObject = Json.obj(
      "email-initiation-request-id" -> "test-request-id",
      "PensionSchemePractitionerId" -> "A2500001",
      "emailAddress" -> "test@test.com",
      "event" -> Sent.toString,
      "submittedBy" -> "PSP",
      "reportVersion" -> "1",
      "PensionSchemeTaxReference" -> "pstr-test"
    )

    event.auditType shouldBe "EventReportingEmailEvent"
    event.details shouldBe expected
  }
}
