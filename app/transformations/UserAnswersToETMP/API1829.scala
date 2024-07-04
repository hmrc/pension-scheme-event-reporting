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

import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer
import play.api.libs.functional.syntax._


object API1829 {
  import transformations.UserAnswersToETMP.API1829Paths._
  import transformations.UserAnswersToETMP.API1829ReadsUtilities._

  val transformToETMPData: Reads[JsObject] = {
    (reqReads(etmpPathToPstr, uaPathToPstr) and
      reqReads(etmpPathToReportStartDate, uaPathToReportStartDate) and
      reqReads(etmpPathToReportEndDate, uaPathToReportEndDate) and
      reqReads(etmpPathToSubmittedBy, uaPathToSubmittedBy) and
      reqReads(etmpPathToSubmittedId, uaPathToSubmittedId) and
      optReads(etmpPathToStartDate, uaPathToSchemeMasterTrustStartDate) and
      optReads(etmpPathToCeaseDate, uaPathToSchemeMasterTrustCeaseDate) and
      readsPsaOrPsp
      ).reduce
  }
}

private object API1829ReadsUtilities extends Transformer {
  import transformations.UserAnswersToETMP.API1829Paths._

  private val readsPsaDeclaration: Reads[JsObject] = {
    (reqReads(etmpPathToPsaDeclaration1, uaPathToPsaDeclaration1) and
      reqReads(etmpPathToPsaDeclaration2, uaPathToPsaDeclaration2)).reduce
  }

  private val readsPspDeclaration: Reads[JsObject] = {
    (reqReads(etmpPathToAuthorisedPSAID, uaPathToAuthorisedPSAID) and
      reqReads(etmpPathToPspDeclaration1, uaPathToPspDeclaration1) and
      reqReads(etmpPathToPspDeclaration2, uaPathToPspDeclaration2)
      ).reduce
  }

  val readsPsaOrPsp: Reads[JsObject] = readsPspDeclaration.orElse(readsPsaDeclaration)

  lazy val reqReads: (JsPath, JsPath) => Reads[JsObject] = (etmpPath: JsPath, uaPath: JsPath) => etmpPath.json.copyFrom(uaPath.json.pick)

  lazy val optReads: (JsPath, JsPath) => Reads[JsObject] = (etmpPath: JsPath, uaPath: JsPath) => etmpPath.json.copyFrom(uaPath.json.pick).orElse(doNothing)
}

private object API1829Paths {
  // ETMP
  private val etmpPathToEventReportDetails:       JsPath = __ \ "eventReportDetails"

  private val etmpPathToEr20aDetails:             JsPath = etmpPathToEventReportDetails \ "er20aDetails"
  val etmpPathToPstr:                             JsPath = etmpPathToEr20aDetails \ "pSTR"
  val etmpPathToReportEndDate:                    JsPath = etmpPathToEr20aDetails \ "reportEndDate"
  val etmpPathToReportStartDate:                  JsPath = etmpPathToEr20aDetails \ "reportStartDate"

  val etmpPathToErDeclarationDetails:             JsPath = etmpPathToEventReportDetails \ "erDeclarationDetails"
  val etmpPathToSubmittedBy:                      JsPath = etmpPathToErDeclarationDetails \ "submittedBy"
  val etmpPathToSubmittedId:                      JsPath = etmpPathToErDeclarationDetails \ "submittedID"

  val etmpPathToPsaDeclaration:                   JsPath = etmpPathToErDeclarationDetails \ "psaDeclaration"
  val etmpPathToPsaDeclaration1:                  JsPath = etmpPathToPsaDeclaration \ "psaDeclaration1"
  val etmpPathToPsaDeclaration2:                  JsPath = etmpPathToPsaDeclaration \ "psaDeclaration2"

  val etmpPathToPspDeclaration:                   JsPath = etmpPathToErDeclarationDetails \ "pspDeclaration"
  val etmpPathToAuthorisedPSAID:                  JsPath = etmpPathToPspDeclaration \ "authorisedPSAID"
  val etmpPathToPspDeclaration1:                  JsPath = etmpPathToPspDeclaration \ "pspDeclaration1"
  val etmpPathToPspDeclaration2:                  JsPath = etmpPathToPspDeclaration \ "pspDeclaration2"

  private val etmpPathToSchemeMasterTrustDetails: JsPath = etmpPathToEventReportDetails \ "schemeMasterTrustDetails"
  val etmpPathToCeaseDate:                        JsPath = etmpPathToSchemeMasterTrustDetails \ "ceaseDate"
  val etmpPathToStartDate:                        JsPath = etmpPathToSchemeMasterTrustDetails \ "startDate"

  // UA
  val uaPathToAuthorisedPSAID:                    JsPath = __ \ "authorisedPSAID"
  val uaPathToPsaDeclaration1:                    JsPath = __ \ "psaDeclaration1"
  val uaPathToPsaDeclaration2:                    JsPath = __ \ "psaDeclaration2"
  val uaPathToPspDeclaration1:                    JsPath = __ \ "pspDeclaration1"
  val uaPathToPspDeclaration2:                    JsPath = __ \ "pspDeclaration2"
  val uaPathToPstr:                               JsPath = __ \ "pstr"
  val uaPathToReportEndDate:                      JsPath = __ \ "reportEndDate"
  val uaPathToReportStartDate:                    JsPath = __ \ "reportStartDate"
  val uaPathToSchemeMasterTrustCeaseDate:         JsPath = __ \ "schemeMasterTrustCeaseDate"
  val uaPathToSchemeMasterTrustStartDate:         JsPath = __ \ "schemeMasterTrustStartDate"
  val uaPathToSubmittedBy:                        JsPath = __ \ "submittedBy"
  val uaPathToSubmittedId:                        JsPath = __ \ "submittedID"

}
