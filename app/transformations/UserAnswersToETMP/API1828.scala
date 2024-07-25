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

package transformations.UserAnswersToETMP

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.{ReadsUtils, Transformer}


object API1828 extends {
  import transformations.UserAnswersToETMP.API1828Paths._
  import transformations.UserAnswersToETMP.API1828ReadsUtilities._

  val transformToETMPData: Reads[JsObject] = {
    (reqReads(etmpPathToPstr, uaPathToPstr) and
      reqReads(etmpPathToReportStartDate, uaPathToReportStartDate) and
      reqReads(etmpPathToReportEndDate, uaPathToReportEndDate) and
      reqReads(etmpPathToSubmittedBy, uaPathToSubmittedBy) and
      reqReads(etmpPathToSubmittedId, uaPathToSubmittedId) and
      readsPsaOrPsp
      ).reduce
  }
}

private object API1828ReadsUtilities extends Transformer with ReadsUtils {
  import transformations.UserAnswersToETMP.API1828Paths._

  private val readsPsaDeclaration: Reads[JsObject] = {
    (reqReads(etmpPathToPsaDeclaration1, uaPathToPsaDeclaration1) and
      reqReads(etmpPathToPsaDeclaration2, uaPathToPsaDeclaration2)).reduce
  }

  private val readsPspDeclaration: Reads[JsObject] = {
    (reqReads(etmpPathToAuthorisedPSAID, uaPathToAuthorisedPSAID) and
      reqReads(etmpPathToPspDeclaration1, uaPathToPspDeclaration1) and
      reqReads(etmpPathToPspDeclaration2, uaPathToPspDeclaration2)).reduce
  }

  val readsPsaOrPsp: Reads[JsObject] = readsPspDeclaration.orElse(readsPsaDeclaration)

}

private object API1828Paths {
  // ETMP
  val etmpPathToDeclarationDetails:     JsPath = __ \ "declarationDetails"
  val etmpPathToErDetails:              JsPath = etmpPathToDeclarationDetails \ "erDetails"
  val etmpPathToErDeclarationDetails:   JsPath = etmpPathToDeclarationDetails \ "erDeclarationDetails"

  val etmpPathToPstr:                   JsPath = etmpPathToErDetails \ "pSTR"
  val etmpPathToReportStartDate:        JsPath = etmpPathToErDetails \ "reportStartDate"
  val etmpPathToReportEndDate:          JsPath = etmpPathToErDetails \ "reportEndDate"

  val etmpPathToSubmittedBy:            JsPath = etmpPathToErDeclarationDetails \ "submittedBy"
  val etmpPathToSubmittedId:            JsPath = etmpPathToErDeclarationDetails \ "submittedID"

  val etmpPathToPsaDeclaration:         JsPath = etmpPathToDeclarationDetails \ "psaDeclaration"
  val etmpPathToPsaDeclaration1:        JsPath = etmpPathToPsaDeclaration \ "psaDeclaration1"
  val etmpPathToPsaDeclaration2:        JsPath = etmpPathToPsaDeclaration \ "psaDeclaration2"

  val etmpPathToPspDeclaration:         JsPath = etmpPathToDeclarationDetails \ "pspDeclaration"
  val etmpPathToAuthorisedPSAID:        JsPath = etmpPathToPspDeclaration \ "authorisedPSAID"
  val etmpPathToPspDeclaration1:        JsPath = etmpPathToPspDeclaration \ "pspDeclaration1"
  val etmpPathToPspDeclaration2:        JsPath = etmpPathToPspDeclaration \ "pspDeclaration2"

  // UA
  val uaPathToPstr:                     JsPath = __ \ "pstr"
  val uaPathToReportStartDate:          JsPath = __ \ "reportStartDate"
  val uaPathToReportEndDate:            JsPath = __ \ "reportEndDate"
  val uaPathToSubmittedBy:              JsPath = __ \ "submittedBy"
  val uaPathToSubmittedId:              JsPath = __ \ "submittedID"
  val uaPathToPsaDeclaration1:          JsPath = __ \ "psaDeclaration1"
  val uaPathToPsaDeclaration2:          JsPath = __ \ "psaDeclaration2"
  val uaPathToAuthorisedPSAID:          JsPath = __ \ "authorisedPSAID"
  val uaPathToPspDeclaration1:          JsPath = __ \ "pspDeclaration1"
  val uaPathToPspDeclaration2:          JsPath = __ \ "pspDeclaration2"
}
