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

package actions

import base.SpecBase
import connectors.SchemeConnector
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatest.BeforeAndAfterEach
import play.api.mvc.{Action, AnyContent, BodyParsers, Results}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.AuditServiceSpec.mock
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.{Name, ~}
import uk.gov.hmrc.domain.{PsaId, PspId}
import uk.gov.hmrc.http.HttpException
import utils.AuthUtils
import utils.AuthUtils.{FakeFailingAuthConnector, psaId, pspId, srn}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthActionSpec extends SpecBase with BeforeAndAfterEach {

  private type RetrievalsType = Enrolments ~ Option[String] ~ Option[Name]

  private class Harness(authAction: AuthAction) {
    def onPageLoad(): Action[AnyContent] = authAction(AuthUtils.srn) { _ => Results.Ok }
  }

  private val mockAuthConnector: AuthConnector = mock[AuthConnector]
  private val mockSchemeConnector = mock[SchemeConnector]
  private lazy val bodyParsers = app.injector.instanceOf[BodyParsers.Default]
  private lazy val action = new AuthAction(mockAuthConnector, bodyParsers, mockSchemeConnector)

  override def beforeEach(): Unit = {
    Mockito.reset(mockAuthConnector)
    Mockito.reset(mockSchemeConnector)
  }

  "PsaPspEnrolmentAuthAction" must {

    "when the user is logged in and has a PODS enrolment" must {

      "must succeed with PSA credentials and scheme access" in {

        running(app) {

          AuthUtils.authStub(mockAuthConnector, mockSchemeConnector)
          val controller = new Harness(action)
          val result = controller.onPageLoad()(FakeRequest())

          status(result) mustEqual OK
        }
      }

      "must succeed with PSP credentials and scheme access" in {

        running(app) {

          AuthUtils.authStubPsp(mockAuthConnector, mockSchemeConnector)
          val controller = new Harness(action)
          val result = controller.onPageLoad()(FakeRequest())

          status(result) mustEqual OK
        }
      }

      "must return Forbidden if no scheme access for PSA" in {
        when(mockAuthConnector.authorise[Enrolments ~ Option[String] ~ Option[Name]](any(), any())(any(), any()))
          .thenReturn(Future.successful(AuthUtils.authResponse))
        when(mockSchemeConnector.checkForAssociation(ArgumentMatchers.eq(Left(PsaId(psaId))), ArgumentMatchers.eq(srn))(any()))
          .thenReturn(Future.successful(Right(false)))

        val controller = new Harness(action)
        val result = controller.onPageLoad()(FakeRequest())

        status(result) mustEqual FORBIDDEN
        contentAsString(result) mustEqual "User is not associated with the scheme"
      }
      "must return Forbidden if no scheme access for PSP" in {
        when(mockAuthConnector.authorise[Enrolments ~ Option[String] ~ Option[Name]](any(), any())(any(), any())) thenReturn
          Future.successful(AuthUtils.authResponsePsp)
        when(mockSchemeConnector.checkForAssociation(ArgumentMatchers.eq(Right(PspId(pspId))), ArgumentMatchers.eq(srn))(any()))
          .thenReturn(Future.successful(Right(false)))

        val controller = new Harness(action)
        val result = controller.onPageLoad()(FakeRequest())

        status(result) mustEqual FORBIDDEN
        contentAsString(result) mustEqual "User is not associated with the scheme"
      }
      "must return internal server errors if association check failed" in {
        when(mockAuthConnector.authorise[Enrolments ~ Option[String] ~ Option[Name]](any(), any())(any(), any())) thenReturn
          Future.successful(AuthUtils.authResponsePsp)
        when(mockSchemeConnector.checkForAssociation(ArgumentMatchers.eq(Right(PspId(pspId))), ArgumentMatchers.eq(srn))(any()))
          .thenReturn(Future.successful(Left(new HttpException("", 500))))

        val controller = new Harness(action)
        val result = controller.onPageLoad()(FakeRequest())

        status(result) mustEqual INTERNAL_SERVER_ERROR
        contentAsString(result) mustEqual "Association check failed"
      }
    }

    "when the user is logged in without a PODS enrolment" must {

      "must return FORBIDDEN" in {

        running(app) {

          when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
            .thenReturn(Future.successful(
              new ~ (
                new~(Enrolments(Set.empty), Some("id")),
                Some(Name(Some("first"), Some("last")))
              )

            ))

          val controller = new Harness(action)
          val result = controller.onPageLoad()(FakeRequest())

          status(result) mustEqual FORBIDDEN
        }
      }
    }

    "when the user is not logged in" must {

      "must return Unauthorized" in {

        running(app) {
          val authAction = new AuthAction(new FakeFailingAuthConnector(new MissingBearerToken), bodyParsers, mockSchemeConnector)
          val controller = new Harness(authAction)
          val result = controller.onPageLoad()(FakeRequest())

          status(result) mustBe UNAUTHORIZED
        }
      }
    }

  }
}
