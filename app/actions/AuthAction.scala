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

import connectors.{SchemeConnector, SessionDataCacheConnector}
import models.SchemeReferenceNumber
import models.enumeration.AdministratorOrPractitioner
import models.enumeration.AdministratorOrPractitioner.{Administrator, Practitioner}
import play.api.Logging
import play.api.mvc.Results._
import play.api.mvc._
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Name, ~}
import uk.gov.hmrc.domain.{PsaId, PspId}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
case class AuthRequest[A](request: Request[A],
                          psaOrPspId:Either[PsaId, PspId],
                          externalId: String,
                          name: Option[Name]) extends WrappedRequest[A](request) {
  def getId: String = psaOrPspId match {
    case Left(psaId) => psaId.id
    case Right(pspId) => pspId.id
  }
}

class AuthActionImpl (
                                  override val authConnector: AuthConnector,
                                  val parser: BodyParsers.Default,
                                  schemeConnector: SchemeConnector,
                                  srn: SchemeReferenceNumber,
                                  sessionDataCacheConnector: SessionDataCacheConnector
                                )(implicit val executionContext: ExecutionContext)
  extends ActionBuilder[AuthRequest, AnyContent]
    with ActionFunction[Request, AuthRequest]
    with AuthorisedFunctions
    with Logging {

  private val PSPEnrolmentKey: String = "HMRC-PODSPP-ORG"
  private val PSPEnrolmentIdKey: String = "PspID"
  private val PSAEnrolmentKey: String = "HMRC-PODS-ORG"
  private val PSAEnrolmentIdKey: String = "PsaID"
  private def getEnrolmentIdentifier(
                                      enrolments: Enrolments,
                                      enrolmentKey: String,
                                      enrolmentIdKey: String
                                    ): Option[String] =
    for {
      enrolment <- enrolments.getEnrolment(enrolmentKey)
      identifier <- enrolment.getIdentifier(enrolmentIdKey)
    }
    yield identifier.value

  override def
  invokeBlock[A](request: Request[A], block: AuthRequest[A] => Future[Result]): Future[Result] =
    invoke(request, block)(HeaderCarrierConverter.fromRequest(request))

  private def administratorOrPractitioner()(implicit hc: HeaderCarrier): Future[Option[AdministratorOrPractitioner]] = {
    sessionDataCacheConnector.fetch().map { optionJsValue =>
      optionJsValue.flatMap { json =>
        (json \ "administratorOrPractitioner").toOption.flatMap(_.validate[AdministratorOrPractitioner].asOpt)
      }
    }
  }

  type PsaOrPspId = Either[PsaId, PspId]

  private def enrolmentResult(enrolments: Enrolments)
                           (block: PsaOrPspId => Future[Result])(implicit hc: HeaderCarrier) = {
    val psaId = getEnrolmentIdentifier(
      enrolments,
      PSAEnrolmentKey,
      PSAEnrolmentIdKey
    )

    val pspId = getEnrolmentIdentifier(
      enrolments,
      PSPEnrolmentKey,
      PSPEnrolmentIdKey
    )

    val psaOrPspIdFtr:Future[Either[PsaOrPspId, String]] = psaId -> pspId match {
        case (None, None) => Future.successful(Right("No credentials found"))
        case (Some(psaId), Some(pspId)) =>
          administratorOrPractitioner().map {
            case None =>
              val msg = "User has both PSA and PSP credentials, information about which one to use for authentication was not provided"
              logger.error(msg)
              Right(msg)
            case Some(Administrator) =>
              Left(Left(PsaId(psaId)))
            case Some(Practitioner) =>
              Left(Right(PspId(pspId)))
          } recover { e =>
            val msg = "Unable to retrive logged in user from PSA service"
            logger.error(msg,e)
            Right(msg)
          }
        case (Some(psaId), None) => Future.successful(Left(Left(PsaId(psaId))))
        case (None, Some(pspId)) => Future.successful(Left(Right(PspId(pspId))))
      }

    psaOrPspIdFtr.flatMap {
      case Right(msg) =>
        Future.successful(Forbidden(msg))
      case Left(psaOrPspId: PsaOrPspId) =>
        block(psaOrPspId)
    }
  }

  private def invoke[A](request: Request[A], block: AuthRequest[A] => Future[Result])
                       (implicit hc: HeaderCarrier): Future[Result] = {
    authorised(Enrolment(PSAEnrolmentKey) or
      Enrolment(PSPEnrolmentKey)).retrieve(
      Retrievals.authorisedEnrolments and
        Retrievals.externalId and
        Retrievals.name) {
      case enrolments ~ Some(externalId) ~ name =>
        enrolmentResult(enrolments) { psaOrPspId =>
          schemeConnector.checkForAssociation(psaOrPspId, srn).flatMap {
            case Right(true) => block(AuthRequest(request, psaOrPspId, externalId, name))
            case Right(false) =>
              logger.warn("User is not associated with the scheme")
              Future.successful(Forbidden("User is not associated with the scheme"))
            case Left(e) =>
              logger.error("Association check failed", e)
              Future.successful(InternalServerError("Association check failed"))
          }
        }
      case _ => Future.failed(new RuntimeException("No externalId found"))
    }
  } recover {
    case e:
      InsufficientEnrolments =>
      logger.warn("Failed to authorise due to insufficient enrolments", e)
      Forbidden("Current user doesn't have a valid enrolment.")
    case e:
      AuthorisationException =>
      logger.warn(s"Failed to authorise", e)
      Unauthorized(s"Failed to authorise user: ${e.reason}")
    case NonFatal(thr) =>
      logger.error(s"Error returned from auth service: ${thr.getMessage}", thr)
      throw thr
  }
}

class AuthAction @Inject()(authConnector: AuthConnector,
                           parser: BodyParsers.Default,
                           schemeConnector: SchemeConnector,
                           sessionDataCacheConnector: SessionDataCacheConnector)(implicit executionContext: ExecutionContext) {
  def apply(srn: SchemeReferenceNumber):ActionBuilder[AuthRequest, AnyContent] & ActionFunction[Request, AuthRequest] =
    new AuthActionImpl(authConnector, parser, schemeConnector, srn, sessionDataCacheConnector)
}