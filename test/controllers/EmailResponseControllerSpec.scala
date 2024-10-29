/*
 * Copyright 2024 HM Revenue & Customs
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

import audit.EmailAuditEvent
import models._
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.Application
import play.api.http.Status.FORBIDDEN
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.{EventReportCacheRepository, ToggleDataRepository}
import services.AuditService
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.crypto.{ApplicationCrypto, PlainText}
import java.time.ZonedDateTime

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
      bind[EventReportCacheRepository].toInstance(mock[EventReportCacheRepository]),
      bind[ToggleDataRepository].toInstance(mock[ToggleDataRepository])
    )).build()

  private val injector = application.injector
  private val controller = injector.instanceOf[EmailResponseController]
  private val encryptedPsaId = injector.instanceOf[ApplicationCrypto].QueryParameterCrypto.encrypt(PlainText(psaOrPspId)).value
  private val encryptedPstr = injector.instanceOf[ApplicationCrypto].QueryParameterCrypto.encrypt(PlainText(pstr)).value
  private val encryptedEmail = injector.instanceOf[ApplicationCrypto].QueryParameterCrypto.encrypt(PlainText(email)).value

  override def beforeEach(): Unit = {
    reset(mockAuditService)
    reset(mockAuthConnector)
    when(mockAuthConnector.authorise[Enrolments](any(), any())(any(), any()))
      .thenReturn(Future.successful(enrolments))
  }

  "EmailResponseController" must {

    "respond OK when given EmailEvents for PSA" in {
      val result = controller.sendAuditEvents(schemeAdministratorTypeAsPsa, requestId, encryptedEmail, encryptedPsaId, encryptedPstr, reportVersion)(fakeRequest.withBody(Json.toJson(emailEvents)))

      status(result) mustBe OK
      verify(mockAuditService, times(4)).sendEvent(eventCaptor.capture())(any(), any())
      eventCaptor.getValue mustEqual EmailAuditEvent(psaOrPspId, pstr, schemeAdministratorTypeAsPsa, email, Complained, requestId, reportVersion)
    }

    "respond OK when given EmailEvents for PSP" in {
      val result = controller.sendAuditEvents(schemeAdministratorTypeAsPsp, requestId, encryptedEmail, encryptedPsaId, encryptedPstr, reportVersion)(fakeRequest.withBody(Json.toJson(emailEvents)))

      status(result) mustBe OK
      verify(mockAuditService, times(4)).sendEvent(eventCaptor.capture())(any(), any())
      eventCaptor.getValue mustEqual EmailAuditEvent(psaOrPspId, pstr, schemeAdministratorTypeAsPsp, email, Complained, requestId, reportVersion)
    }

    "respond with BAD_REQUEST when not given EmailEvents" in {
      val result = controller.sendAuditEvents(
        schemeAdministratorTypeAsPsp, requestId, encryptedEmail, encryptedPsaId, encryptedPstr, reportVersion)(fakeRequest.withBody(Json.obj("name" -> "invalid")))

      verify(mockAuditService, never).sendEvent(any())(any(), any())
      status(result) mustBe BAD_REQUEST
    }

    "decryptPsaOrPspIdAndEmail" should {
      "return IllegalArgumentException when email does not match regex" in {
        val invalidEmail = "test.com"
        val encryptedInvalidEmail = injector.instanceOf[ApplicationCrypto].QueryParameterCrypto.encrypt(PlainText(invalidEmail)).value

        val result = controller.sendAuditEvents(
          schemeAdministratorTypeAsPsp,
          requestId,
          encryptedInvalidEmail,
          encryptedPsaId,
          encryptedPstr,
          reportVersion)(fakeRequest.withBody(Json.toJson(emailEvents)))

        status(result) mustBe FORBIDDEN

      }
    }
  }
}

object EmailResponseControllerSpec {
  private val psaOrPspId = "A7654321"
  private val pstr = "87219363YN"
  private val schemeAdministratorTypeAsPsa = "PSA"
  private val schemeAdministratorTypeAsPsp = "PSP"
  private val email = "test@test.com"
  private val requestId = "test-request-id"
  private val reportVersion = "1"
  private val fakeRequest = FakeRequest("", "")
  private val enrolments = Enrolments(Set(
    Enrolment("HMRC-PODS-ORG", Seq(
      EnrolmentIdentifier("PSAID", "A0000000")
    ), "Activated", None)
  ))
  private val eventCaptor = ArgumentCaptor.forClass(classOf[EmailAuditEvent])
  private val emailEvents = EmailEvents(Seq(EmailEvent(Sent, now()), EmailEvent(Delivered, now()),
    EmailEvent(PermanentBounce, now()), EmailEvent(Opened, now()), EmailEvent(Complained, now())))

  def now(): ZonedDateTime = ZonedDateTime.now()
}
