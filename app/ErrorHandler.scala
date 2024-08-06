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

import models.{ErrorResult, UserLockedError, UserLockedException}

import javax.inject.Singleton
import scala.concurrent._
import play.api.http.HttpErrorHandler
import play.api.http.Status.FORBIDDEN
import play.api.mvc._
import play.api.mvc.Results._

@Singleton
class ErrorHandler extends HttpErrorHandler {
  def onClientError(request: RequestHeader, statusCode: Int, message: String): Future[Result] = {
    Future.successful(
      Status(statusCode)("A client error occurred: " + message)
    )
  }

  def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = {
    exception match {
      case e:UserLockedException =>
        Future.successful(
          ErrorResult(FORBIDDEN, UserLockedError("EVENT_LOCKED", e.psaOrPspId))
        )
      case _ =>
        Future.successful(
          InternalServerError("A server error occurred: " + exception.getMessage)
        )
    }
  }
}