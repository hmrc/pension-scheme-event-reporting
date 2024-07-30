package models

import play.api.libs.json.{Json, OFormat, Writes}
import play.api.mvc.{Result, Results}

object ErrorResult extends Results {
  def apply[T <: ErrorResponse](status: Int, body: T)(implicit reads: Writes[T]): Result = new Status(status)(Json.toJson(body))
}

private class ErrorResponse(message: String)

case class UserLockedError(
                               message: String,
                               lockedByName: String
                             ) extends ErrorResponse(message)

object UserLockedError {
  implicit val format: OFormat[UserLockedError] = Json.format
}