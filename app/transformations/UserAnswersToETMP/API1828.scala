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

import transformations.Transformer
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

object API1828 extends Transformer {
  val transformToETMPData: Reads[JsObject] = {

    val submitEventDeclaration = ((__ \ "pSTR").read[String] and
      (__ \ "reportStartDate").read[String] and
      (__ \ "reportEndDate").read[String] and
      (__ \ "submittedBy").read[String] and
      (__ \ "submittedID").read[String]
    ).reduce
  }
}