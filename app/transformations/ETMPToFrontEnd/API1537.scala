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

package transformations.ETMPToFrontEnd

import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer

object API1537 extends Transformer {

  private val test: JsValue => JsString = {
    case JsString("SubmittedAndInProgress") => JsString("submitted")
    case JsString("SubmittedAndSuccessfullyProcessed") => JsString("submitted")
    case JsString("Compiled") => JsString("compiled")
    case e => throw new RuntimeException("Not a string: " + e)
  }

  private val dateFormatter: JsValue => JsString = {
    case JsString(x) => JsString(x.take(10))
    case e => throw new RuntimeException("Incorrect date string format: " + e)
  }

  private val readsSubmitter: Reads[JsString] =
    (
      (__ \ "reportSubmitterDetails" \ "organisationOrPartnershipDetails" \ "organisationOrPartnershipName").readNullable[String] and
        (__ \ "reportSubmitterDetails" \ "individualDetails" \ "firstName").readNullable[String] and
        (__ \ "reportSubmitterDetails" \ "individualDetails" \ "lastName").readNullable[String]
      )(
      (orgName, firstName, lastName) =>
        (orgName, firstName, lastName) match {
          case (None, Some(fn), Some(ln)) => Reads.pure(JsString(s"$fn $ln"))
          case (Some(o), None, None) => Reads.pure(JsString(o))
          case _ => fail[JsString]
        }
    ).flatMap(identity)

  private val readsDetail = (
    (__ \ "versionDetails" \ "version").json.copyFrom((__ \ "reportVersion").json.pick) and
      (__ \ "versionDetails" \ "status").json.copyFrom((__ \ "reportStatus").json.pick.map(test)) and
      (__ \ "submittedDate").json.copyFrom((__ \ "compilationOrSubmissionDate").json.pick.map(dateFormatter)) and
      (__ \ "submitterName").json.copyFrom(readsSubmitter)).reduce

  val reads: Reads[JsArray] = Reads.seq(readsDetail).map(JsArray(_))
}
