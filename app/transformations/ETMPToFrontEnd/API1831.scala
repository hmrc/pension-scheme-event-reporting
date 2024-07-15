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

import play.api.libs.json._
import transformations.Transformer
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._


object API1831 {

  import API1831Paths._
  import transformations.ETMPToFrontEnd.Event20AReadsUtilities._

  implicit val rds1831Api: Reads[JsObject] = (
    reqReads(uaPstr, etmpPstr)                                                    and
      reqReads(uaReportStartDate, etmpReportStartDate)                              and
      reqReads(uaReportEndDate, etmpReportEndDate)                                  and
      reqReads(uaSubmittedBy, etmpSubmittedBy)                                      and
      reqReads(uaSubmittedID, etmpSubmittedID)                                      and
      optReads(uaSchemeMasterTrustStartDate, etmpSchemeMasterTrustDetailsStartDate) and
      optReads(uaSchemeMasterTrustCeaseDate, etmpSchemeMasterTrustDetailsCeaseDate) and
      optReads(uaPsaDeclaration1, etmpPsaDeclaration1)                              and
      optReads(uaPsaDeclaration2, etmpPsaDeclaration2)                              and
      optReads(uaAuthorisedPSAID, etmpAuthorisedPSAID)                              and
      optReads(uaPspDeclaration1, etmpPspDeclaration1)                              and
      optReads(uaPspDeclaration2, etmpPspDeclaration2)
    ).reduce
}

private object API1831Paths {
  // UA
  val uaPstr:                                 JsPath = __ \ "pstr"
  val uaReportStartDate:                      JsPath = __ \ "reportStartDate"
  val uaReportEndDate:                        JsPath = __ \ "reportEndDate"
  val uaSubmittedBy:                          JsPath = __ \ "submittedBy"
  val uaSubmittedID:                          JsPath = __ \ "submittedID"
  val uaSchemeMasterTrustStartDate:           JsPath = __ \ "schemeMasterTrustStartDate"
  val uaSchemeMasterTrustCeaseDate:           JsPath = __ \ "schemeMasterTrustCeaseDate"
  val uaPsaDeclaration1:                      JsPath = __ \ "psaDeclaration1"
  val uaPsaDeclaration2:                      JsPath = __ \ "psaDeclaration2"
  val uaAuthorisedPSAID:                      JsPath = __ \ "authorisedPSAID"
  val uaPspDeclaration1:                      JsPath = __ \ "pspDeclaration1"
  val uaPspDeclaration2:                      JsPath = __ \ "pspDeclaration2"

  // ETMP
  val etmpPstr:                               JsPath = __ \ "schemeDetails" \ "pSTR"
  val etmpReportStartDate:                    JsPath = __ \ "er20aDetails" \ "reportStartDate"
  val etmpReportEndDate:                      JsPath = __ \ "er20aDetails" \ "reportEndDate"
  val etmpSubmittedBy:                        JsPath = __ \ "erDeclarationDetails" \ "submittedBy"
  val etmpSubmittedID:                        JsPath = __ \ "erDeclarationDetails" \ "submittedID"
  val etmpSchemeMasterTrustDetailsStartDate:  JsPath = __ \ "schemeMasterTrustDetails" \ "startDate"
  val etmpSchemeMasterTrustDetailsCeaseDate:  JsPath = __ \ "schemeMasterTrustDetails" \ "ceaseDate"
  val etmpPsaDeclaration1:                    JsPath = __ \ "erDeclarationDetails" \ "psaDeclaration" \ "psaDeclaration1"
  val etmpPsaDeclaration2:                    JsPath = __ \ "erDeclarationDetails" \ "psaDeclaration" \ "psaDeclaration2"
  val etmpAuthorisedPSAID:                    JsPath = __ \ "erDeclarationDetails" \ "pspDeclaration" \ "authorisedPSAID"
  val etmpPspDeclaration1:                    JsPath = __ \ "erDeclarationDetails" \ "pspDeclaration" \ "pspDeclaration1"
  val etmpPspDeclaration2:                    JsPath = __ \ "erDeclarationDetails" \ "pspDeclaration" \ "pspDeclaration2"
}

private object Event20AReadsUtilities extends Transformer {
  lazy val reqReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick)

  lazy val optReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick).orElse(doNothing)
}