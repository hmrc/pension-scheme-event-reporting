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

import play.api.libs.json.{Format, Json}

case class ReportVersion(
                        versionDetails: VersionDetails,
                        submittedDate: String,
                        submitterName: Option[String]
                        )

object ReportVersion {
  implicit val formats: Format[ReportVersion] = Json.format[ReportVersion]

  implicit def ordering[A <: ReportVersion]: Ordering[A] = (x: A, y: A) =>
    x.versionDetails.version.compare(y.versionDetails.version)
}


case class VersionDetails(
                         version: Int,
                         status: String
                         )

object VersionDetails {
  implicit val formats: Format[VersionDetails] = Json.format[VersionDetails]
}
