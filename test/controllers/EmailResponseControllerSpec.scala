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

package controllers

import audit.{AuditService, EmailAuditEvent}
import models._
import models.enumeration.JourneyType.AFT_SUBMIT_RETURN
import models.enumeration.SchemeAdministratorType
import org.joda.time.DateTime
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{never, times, verify, when}
import org.mockito.{ArgumentCaptor, Mockito}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repository._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.crypto.{ApplicationCrypto, PlainText}

import scala.concurrent.Future

class EmailResponseControllerSpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfterEach { // scalastyle:off magic.number

  import EmailResponseControllerSpec._

  private val mockAuditService = mock[AuditService]
  private val mockAuthConnector = mock[AuthConnector]

  private val application: Application = new GuiceApplicationBuilder()
    .configure(conf = "auditing.enabled" -> false, "metrics.enabled" -> false, "metrics.jvm" -> false).
    overrides(Seq(
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[AuditService].toInstance(mockAuditService),
      bind[AdminDataRepository].toInstance(mock[AdminDataRepository]),
      bind[AftBatchedDataCacheRepository].toInstance(mock[AftBatchedDataCacheRepository]),
      bind[AftOverviewCacheRepository].toInstance(mock[AftOverviewCacheRepository]),
      bind[FileUploadReferenceCacheRepository].toInstance(mock[FileUploadReferenceCacheRepository]),
      bind[FileUploadOutcomeRepository].toInstance(mock[FileUploadOutcomeRepository]),
      bind[FinancialInfoCacheRepository].toInstance(mock[FinancialInfoCacheRepository]),
      bind[FinancialInfoCreditAccessRepository].toInstance(mock[FinancialInfoCreditAccessRepository])
    )).build()

  private val injector = application.injector
  private val controller = injector.instanceOf[EmailResponseController]
  private val encryptedPsaId = injector.instanceOf[ApplicationCrypto].QueryParameterCrypto.encrypt(PlainText(psa)).value
  private val encryptedPspId = injector.instanceOf[ApplicationCrypto].QueryParameterCrypto.encrypt(PlainText(psp)).value
  private val encryptedEmail = injector.instanceOf[ApplicationCrypto].QueryParameterCrypto.encrypt(PlainText(email)).value

  override def beforeEach(): Unit = {
    Mockito.reset(mockAuditService)
    Mockito.reset(mockAuthConnector)
    when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
      .thenReturn(Future.successful(enrolments))
  }

  "EmailResponseController" must {

    "respond OK when given EmailEvents for PSA" in {
      val result = controller.sendAuditEvents(
        requestId, encryptedPsaId, SchemeAdministratorType.PSA, encryptedEmail, AFT_SUBMIT_RETURN)(fakeRequest.withBody(Json.toJson(emailEvents)))

      status(result) mustBe OK
      verify(mockAuditService, times(4)).sendEvent(eventCaptor.capture())(any(), any())
      eventCaptor.getValue mustEqual EmailAuditEvent(psa, SchemeAdministratorType.PSA, email, Complained, AFT_SUBMIT_RETURN, requestId)
    }

    "respond OK when given EmailEvents for PSP" in {
      val result = controller.sendAuditEvents(
        requestId, encryptedPspId, SchemeAdministratorType.PSP, encryptedEmail, AFT_SUBMIT_RETURN)(fakeRequest.withBody(Json.toJson(emailEvents)))

      status(result) mustBe OK
      verify(mockAuditService, times(4)).sendEvent(eventCaptor.capture())(any(), any())
      eventCaptor.getValue mustEqual EmailAuditEvent(psp, SchemeAdministratorType.PSP, email, Complained, AFT_SUBMIT_RETURN, requestId)
    }

    "respond with BAD_REQUEST when not given EmailEvents" in {
      val result = controller.sendAuditEvents(
        requestId, encryptedPsaId, SchemeAdministratorType.PSA, encryptedEmail, AFT_SUBMIT_RETURN)(fakeRequest.withBody(Json.obj("name" -> "invalid")))

      verify(mockAuditService, never).sendEvent(any())(any(), any())
      status(result) mustBe BAD_REQUEST
    }
  }
}

object EmailResponseControllerSpec {
  private val psa = "A7654321"
  private val psp = "21111111"
  private val email = "test@test.com"
  private val requestId = "test-request-id"
  private val fakeRequest = FakeRequest("", "")
  private val enrolments = Enrolments(Set(
    Enrolment("HMRC-PODS-ORG", Seq(
      EnrolmentIdentifier("PSAID", "A0000000")
    ), "Activated", None)
  ))
  private val eventCaptor = ArgumentCaptor.forClass(classOf[EmailAuditEvent])
  private val emailEvents = EmailEvents(Seq(EmailEvent(Sent, DateTime.now()), EmailEvent(Delivered, DateTime.now()),
    EmailEvent(PermanentBounce, DateTime.now()), EmailEvent(Opened, DateTime.now()), EmailEvent(Complained, DateTime.now())))
}
