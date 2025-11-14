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

package utils

import actions.{AuthAction, AuthRequest}
import com.google.inject.Inject
import connectors.{SchemeConnector, SessionDataCacheConnector}
import models.SchemeReferenceNumber
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.mockito.stubbing.OngoingStubbing
import play.api.mvc._
import services.AuditServiceSpec.mock
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.{Name, Retrieval, ~}
import uk.gov.hmrc.domain.{PsaId, PspId}
import uk.gov.hmrc.http.{HeaderCarrier, HttpException}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

object AuthUtils {
  val id = "id"
  val psaId = "A2123456"
  val pspId = "21000005"
  val srn: SchemeReferenceNumber = SchemeReferenceNumber("S2400000006")
  val externalId = "externalId"

  def failedAuthStub(mockAuthConnector: AuthConnector): OngoingStubbing[Future[Unit]] =
    when(mockAuthConnector.authorise[Unit](any(), any())(any(), any())).thenReturn (Future.failed(InsufficientEnrolments()))

  def authStub(mockAuthConnector: AuthConnector, mockSchemeConnector: SchemeConnector): OngoingStubbing[Future[Either[HttpException, Boolean]]] = {
    when(mockAuthConnector.authorise[Enrolments ~ Option[String]](any(), any())(any(), any()))
      .thenReturn(Future.successful(AuthUtils.authResponse))
    when(mockSchemeConnector.checkForAssociation(ArgumentMatchers.eq(Left(PsaId(psaId))), ArgumentMatchers.eq(srn))(any()))
      .thenReturn(Future.successful(Right(true)))
  }

  val authResponse: Enrolments ~ Some[String] = {
    new ~(
      Enrolments(
        Set(
          new Enrolment("HMRC-PODS-ORG", Seq(EnrolmentIdentifier("PsaId", psaId)), "Activated")
        )
      ), Some(id)
    )
  }

  def authStubPsp(mockAuthConnector: AuthConnector, mockSchemeConnector: SchemeConnector): OngoingStubbing[Future[Either[HttpException, Boolean]]] = {
    when(mockAuthConnector.authorise[Enrolments ~ Option[String]](any(), any())(any(), any()))
      .thenReturn(Future.successful(AuthUtils.authResponsePsp))
    when(mockSchemeConnector.checkForAssociation(ArgumentMatchers.eq(Right(PspId(pspId))), ArgumentMatchers.eq(srn))(any()))
      .thenReturn(Future.successful(Right(true)))
  }

  val authResponsePsp = new ~(
    Enrolments(
      Set(
        new Enrolment("HMRC-PODSPP-ORG", Seq(EnrolmentIdentifier("PspId", pspId)), "Activated")
      )
    ), Some(id)
  )

  val authResponsePsaPsp = new ~(
    Enrolments(
      Set(
        new Enrolment("HMRC-PODS-ORG", Seq(EnrolmentIdentifier("PsaId", psaId)), "Activated"),
        new Enrolment("HMRC-PODSPP-ORG", Seq(EnrolmentIdentifier("PspId", pspId)), "Activated")
      )
    ), Some(id)
  )


  def noEnrolmentAuthStub(mockAuthConnector: AuthConnector): OngoingStubbing[Future[Option[String]]] =
    when(mockAuthConnector.authorise[Option[String]](any(), any())(any(), any()))
      .thenReturn(Future.successful(AuthUtils.noEnrolmentAuthResponse))


  val noEnrolmentAuthResponse: Option[String] = Some(externalId)

  private val mockAuthConnector = mock[AuthConnector]
  private val mockParser = mock[BodyParsers.Default]
  private val mockSchemeConnector = mock[SchemeConnector]
  class FakeAuthAction extends AuthAction(mockAuthConnector, mockParser, mockSchemeConnector, mock[SessionDataCacheConnector]) {
    override def apply(srn: SchemeReferenceNumber): ActionBuilder[AuthRequest, AnyContent] & ActionFunction[Request, AuthRequest] = {
      new ActionBuilder[AuthRequest, AnyContent] {
        override def parser: BodyParser[AnyContent] = mockParser

        override def invokeBlock[A](request: Request[A], block: AuthRequest[A] => Future[Result]): Future[Result] =
          block(AuthRequest(request, Left(PsaId(AuthUtils.psaId)),AuthUtils.externalId, Some(Name(Some("first"), Some("last")))))

        override protected def executionContext: ExecutionContext = global
      }
    }
  }

  class FakeFailingAuthConnector @Inject()(exceptionToReturn: Throwable) extends AuthConnector {
    val serviceUrl: String = ""

    override def authorise[A](predicate: Predicate, retrieval: Retrieval[A])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[A] =
      Future.failed(exceptionToReturn)
  }
}
