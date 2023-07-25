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


// Some([{"versionInfo":{"version":1,"status":"submitted"},"submitterName":"ABC Limited"}]) was not equal to
// Some([{"versionInfo":{"version":1,"status":"submitted","submitterName":"ABC Limited"}}]) (API1537Spec.scala:60)
object API1537 {

  private val test: Reads[JsString] = jsVal => jsVal match {
    case JsString("SubmittedAndInProgress") => Reads.pure(JsString("submitted"))
    case JsString("SubmittedAndSuccessfullyProcessed") => (JsString("submitted"))
    case JsString("Compiled") => JsString("compiled")
    case _ => JsError()
  }

  private val readsDetail = (
    (__ \ "versionInfo" \ "version").json.copyFrom((__ \ "reportVersion").json.pick) and
    (__ \ "versionInfo" \ "status").json.copyFrom((__ \ "reportStatus").json.pick.map(test)) and
    (__ \ "submitterName").json.copyFrom((__ \ "reportSubmitterDetails" \ "orgOrPartnershipDetails" \ "orgOrPartnershipName" ).json.pick)).reduce

  val reads: Reads[JsArray] = Reads.seq(readsDetail).map(JsArray(_))



    //[
  //  {
  //    "reportFormBundleNumber": "123456785015",
  //    "reportVersion": 1,
  //    "reportStatus": "Submitted",
  //    "compilationOrSubmissionDate": "2021-04-01T09:30:47Z",
  //    "reportSubmitterDetails": {
  //      "reportSubmittedBy": "PSP",
  //      "orgOrPartnershipDetails": {
  //        "orgOrPartnershipName": "ABC Limited"
  //      }
  //    },
  //    "psaDetails": {
  //      "psaOrgOrPartnershipDetails": {
  //        "orgOrPartnershipName": "XYZ Limited"
  //      }
  //    }
  //  }
  //]
}

