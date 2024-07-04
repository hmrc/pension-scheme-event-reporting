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

package transformations.ETMPToFrontEnd

import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.ETMPToFrontEnd.API1537Paths._
import transformations.Transformer

object API1537 {
  import transformations.ETMPToFrontEnd.API1537ReadsUtilities._

  val reads: Reads[JsArray] = Reads.seq(readsDetail).map(JsArray(_))
}

private object API1537ReadsUtilities extends Transformer {
  private val statusNode: JsValue => JsString = {
    case JsString("SubmittedAndInProgress") => JsString("submitted")
    case JsString("SubmittedAndSuccessfullyProcessed") => JsString("submitted")
    case JsString("Compiled") => JsString("compiled")
    case e => throw new RuntimeException("Not a string: " + e)
  }

  private val dateFormatter: JsValue => JsString = {
    case JsString(dateStr) => JsString(dateStr.take(10))
    case e => throw new RuntimeException("Incorrect date string format: " + e)
  }

  private val readsSubmitter: Reads[JsString] = (
        uaOrganisationOrPartnershipNamePath.readNullable[String] and
        uaFirstNamePath.readNullable[String] and
        uaLastNamePath.readNullable[String]
      )(
      (orgName, firstName, lastName) =>
        (orgName, firstName, lastName) match {
          case (None, Some(fn), Some(ln)) => Reads.pure(JsString(s"$fn $ln"))
          case (Some(o), None, None) => Reads.pure(JsString(o))
          case _ => fail[JsString]
        }
    ).flatMap(identity)

  val readsDetail: Reads[JsObject] = (
    reqReads(uaVersion, etmpReportVersion) and
    reqNestedReads(uaStatus, etmpReportStatus.json.pick.map(statusNode)) and
    reqNestedReads(uaSubmittedDate, etmpCompilationOrSubmissionDate.json.pick.map(dateFormatter)) and
    reqNestedReads(uaSubmitterName, readsSubmitter)
    ).reduce

  private lazy val reqReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick)
  private lazy val reqNestedReads: (JsPath, Reads[JsString]) => Reads[JsObject] =
    (uaPath: JsPath, etmpReads: Reads[JsString]) => uaPath.json.copyFrom(etmpReads)
}

private object API1537Paths {
  // UA
  val uaOrganisationOrPartnershipNamePath:  JsPath = __ \ "reportSubmitterDetails" \ "organisationOrPartnershipDetails" \ "organisationOrPartnershipName"
  private val uaIndividualDetails:          JsPath = __ \ "reportSubmitterDetails" \ "individualDetails"
  val uaFirstNamePath:                      JsPath = uaIndividualDetails \ "firstName"
  val uaLastNamePath:                       JsPath = uaIndividualDetails \ "lastName"

  private val uaVersionDetails:             JsPath = __ \ "versionDetails"
  val uaStatus:                             JsPath = uaVersionDetails \ "status"
  val uaVersion:                            JsPath = uaVersionDetails \ "version"
  val uaSubmittedDate:                      JsPath = __ \ "submittedDate"
  val uaSubmitterName:                      JsPath = __ \ "submitterName"

  // ETMP
  val etmpCompilationOrSubmissionDate:      JsPath = __ \ "compilationOrSubmissionDate"
  val etmpReportStatus:                     JsPath = __ \ "reportStatus"
  val etmpReportVersion:                    JsPath = __ \ "reportVersion"
}
