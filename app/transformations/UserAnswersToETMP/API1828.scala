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

    /*
    val fullExpectedResult = Json.obj(
      "declarationDetails" -> Json.obj(
        "erDetails" -> Json.obj(
          "pSTR" -> pstr,
          "reportStartDate" -> startDate,
          "reportEndDate" -> endDate,
        ),
        "erDeclarationDetails" -> Json.obj(
          "submittedBy" -> psa,
          "submittedID" -> psaOrPspId
        ),
        "psaDeclaration" -> Json.obj( // TODO: amend to be psaDec or pspDec when that functionality is required.
          "psaDeclaration1" -> "Selected",
          "psaDeclaration2" -> "Selected"
        )
      )
    )
     */

    (
      (__ \ "declarationDetails" \ "erDetails" \ "pSTR").json.copyFrom((__ \ "pstr").json.pick) and
        (__ \ "declarationDetails" \ "erDetails" \ "reportStartDate").json.copyFrom((__ \ "reportStartDate").json.pick)
      ).reduce

  }
}
