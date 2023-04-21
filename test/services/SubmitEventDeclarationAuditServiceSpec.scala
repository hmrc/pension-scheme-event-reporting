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

import base.SpecBase
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{reset, times, verify}
import org.scalatest.BeforeAndAfterEach
import play.api.http.Status
import play.api.libs.json.Json
import play.api.mvc.RequestHeader
import play.api.test.FakeRequest
import services.AuditServiceSpec.mock
import uk.gov.hmrc.http.HttpResponse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
class SubmitEventDeclarationAuditServiceSpec extends SpecBase with BeforeAndAfterEach {

  private implicit lazy val rh: RequestHeader = FakeRequest("", "")

  private val mockAuditService = mock[AuditService]

  private val pstr = "pstr"
  private val data = Json.obj("test" -> "test")

  override def beforeEach(): Unit = {
    reset(mockAuditService)
  }

  "SubmitEventDeclarationAuditService" must {
    "send the correct audit event for a successful" in {
      val service = new SubmitEventDeclarationAuditService(mockAuditService)
      val pf = service.sendSubmitEventDeclarationAuditEvent(pstr, data)
      pf(Success(HttpResponse.apply(200, "")))
      val expectedAuditEvent = SubmitEventDeclarationAuditEvent(pstr, Status.OK, data, None)
      verify(mockAuditService, times(1)).sendEvent(expectedAuditEvent)(any(), any())
    }

  }
}
