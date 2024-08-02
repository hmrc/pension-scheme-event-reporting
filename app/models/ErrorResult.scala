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

package models

import play.api.http.Status.FORBIDDEN
import play.api.libs.json.{Json, OFormat, Writes}
import play.api.mvc.{Result, Results}
import uk.gov.hmrc.http.HttpException

object ErrorResult extends Results {
  def apply[T <: ErrorResponse](status: Int, body: T)(implicit reads: Writes[T]): Result = new Status(status)(Json.toJson(body))
}

private class ErrorResponse(message: String)

case class UserLockedException(psaOrPspId: Option[String]) extends HttpException("EVENT_LOCKED", FORBIDDEN)
case class UserLockedError(
                               message: String,
                               lockedByPsaOrPspId: Option[String]
                             ) extends ErrorResponse(message)

object UserLockedError {
  implicit val format: OFormat[UserLockedError] = Json.format
}