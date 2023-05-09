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

import play.api.Logging
import play.api.libs.json.Json
import play.api.mvc._
import repositories.FileUploadResponseCacheRepository
import uk.gov.hmrc.http.{Request => _, _}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}


@Singleton()
class FileUploadOutcomeController @Inject()(
                                             cc: ControllerComponents,
                                             fileUploadResponseCacheRepository: FileUploadResponseCacheRepository
                                           )(implicit ec: ExecutionContext)
  extends BackendController(cc)
    with HttpErrorFunctions
    with Results
    with Logging {

  def save: Action[AnyContent] = Action.async {
    implicit request =>
        request.body.asJson match {
          case Some(json) =>
            val reference = (json \ "reference").as[String]
            logger.debug(message = s"[Save file upload outcome: Incoming-Payload]$reference")
            fileUploadResponseCacheRepository.upsert(reference, json).map(_ => Ok)
          case None =>
            throw new RuntimeException("No JSON body")
        }
  }

    def get: Action[AnyContent] = Action.async {
      implicit request =>
        withReferenceId { reference =>
          fileUploadResponseCacheRepository.get(reference).map {
            case Some(value) => Ok(value)
            case None => NotFound
          }
        }
    }

  private def withReferenceId(block: String => Future[Result])
                             (implicit request: Request[AnyContent]): Future[Result] = {
    request.headers.get("reference") match {
      case Some(id) => block(id)
      case _ => Future.failed(new BadRequestException(s"Bad Request with missing reference"))
    }
  }
}

