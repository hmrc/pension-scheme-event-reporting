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

package transformations.UserAnswersToETMP

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer

object API1828 extends Transformer {

  val transformToETMPData: Reads[JsObject] = {
    (__ \ "submittedBy").read[String] match {
      case "PSA" =>
        (
          (__ \ "declarationDetails" \ "erDetails" \ "pSTR").json.copyFrom((__ \ "pstr").json.pick) and
            (__ \ "declarationDetails" \ "erDetails" \ "reportStartDate").json.copyFrom((__ \ "reportStartDate").json.pick) and
            (__ \ "declarationDetails" \ "erDetails" \ "reportEndDate").json.copyFrom((__ \ "reportEndDate").json.pick) and
            (__ \ "declarationDetails" \ "erDeclarationDetails" \ "submittedBy").json.copyFrom((__ \ "submittedBy").json.pick) and
            (__ \ "declarationDetails" \ "erDeclarationDetails" \ "submittedID").json.copyFrom((__ \ "submittedID").json.pick) and
            (__ \ "declarationDetails" \ "psaDeclaration" \ "psaDeclaration1").json.copyFrom((__ \ "psaDeclaration1").json.pick) and
            (__ \ "declarationDetails" \ "psaDeclaration" \ "psaDeclaration2").json.copyFrom((__ \ "psaDeclaration2").json.pick)
          ).reduce
      case "PSP" =>
        (
          (__ \ "declarationDetails" \ "erDetails" \ "pSTR").json.copyFrom((__ \ "pstr").json.pick) and
            (__ \ "declarationDetails" \ "erDetails" \ "reportStartDate").json.copyFrom((__ \ "reportStartDate").json.pick) and
            (__ \ "declarationDetails" \ "erDetails" \ "reportEndDate").json.copyFrom((__ \ "reportEndDate").json.pick) and
            (__ \ "declarationDetails" \ "erDeclarationDetails" \ "submittedBy").json.copyFrom((__ \ "submittedBy").json.pick) and
            (__ \ "declarationDetails" \ "erDeclarationDetails" \ "submittedID").json.copyFrom((__ \ "submittedID").json.pick) and
            (__ \ "declarationDetails" \ "pspDeclaration" \ "pspDeclaration1").json.copyFrom((__ \ "pspDeclaration1").json.pick) and
            (__ \ "declarationDetails" \ "pspDeclaration" \ "pspDeclaration2").json.copyFrom((__ \ "pspDeclaration2").json.pick) and
            (__ \ "declarationDetails" \ "pspDeclaration" \ "authorisedPSAID").json.copyFrom((__ \ "authorisedPSAID").json.pick)
          ).reduce
    }
  }
}
