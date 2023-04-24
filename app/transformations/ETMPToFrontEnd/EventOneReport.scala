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

import play.api.libs.json._
import transformations.Transformer
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._

//noinspection ScalaStyle
object EventOneReport {

  import Paths._
  import ReadsUtilities._

  
  implicit val rds1833Api: Reads[JsObject] =
    dummyPath.json.copyFrom(pathEtmpEvent1Details.read(readsEvent1Details))

  private lazy val readsEvent1Details: Reads[JsArray] = __.read(Reads.seq(
        (readsMemberType and readsIndividualMemberDetails).reduce
      ).map(JsArray(_)))
}

private object ReadsUtilities extends Transformer {

  import Paths._

  val readsMemberType: Reads[JsObject] = {
    pathUAMemberType.json.copyFrom(pathEtmpMemberType.json.pick)
  }

  val readsIndividualOrEmployerMemberDetails: Unit = {
    // TODO: implement.
  }

  val readsIndividualMemberDetails: Reads[JsObject] = {(
    pathUAIndividualMemberDetailsFirstName.json.copyFrom(pathEtmpIndividualMemberDetailsFirstName.json.pick) and
      pathUAIndividualMemberDetailsLastName.json.copyFrom(pathEtmpIndividualMemberDetailsLastName.json.pick) and
      pathUAIndividualMemberDetailsNino.json.copyFrom(pathEtmpIndividualMemberDetailsNino.json.pick) and
      pathUAIndividualMemberDetailsSignedMandate.json.copyFrom(pathEtmpIndividualMemberDetailsSignedMandate.json.pick) and
      pathUAIndividualMemberDetailsPmtMoreThan25PerFundValue.json.copyFrom(pathEtmpIndividualMemberDetailsPmtMoreThan25PerFundValue.json.pick) and
      pathUAIndividualMemberDetailsSchemePayingSurcharge.json.copyFrom(pathEtmpIndividualMemberDetailsSchemePayingSurcharge.json.pick)
    ).reduce
  }

  // TODO: incorrect implementation.
  val readsEmployerMemberDetails: Reads[JsObject] = {(
    pathUAIndividualMemberDetailsFirstName.json.copyFrom(pathEtmpIndividualMemberDetailsFirstName.json.pick) and
      pathUAIndividualMemberDetailsLastName.json.copyFrom(pathEtmpIndividualMemberDetailsLastName.json.pick) and
      pathUAIndividualMemberDetailsNino.json.copyFrom(pathEtmpIndividualMemberDetailsNino.json.pick) and
      pathUAIndividualMemberDetailsSignedMandate.json.copyFrom(pathEtmpIndividualMemberDetailsSignedMandate.json.pick) and
      pathUAIndividualMemberDetailsPmtMoreThan25PerFundValue.json.copyFrom(pathEtmpIndividualMemberDetailsPmtMoreThan25PerFundValue.json.pick) and
      pathUAIndividualMemberDetailsSchemePayingSurcharge.json.copyFrom(pathEtmpIndividualMemberDetailsSchemePayingSurcharge.json.pick)
    ).reduce
  }
}

private object Paths {

  /* UserAnswers */
  // TODO: these will need to be amended to match the actual UA structure.
  val dummyPath: JsPath = __ \ Symbol("dummy")
  val pathUAMemberType: JsPath = __ \ Symbol("memberType")
  val pathUAIndividualMemberDetailsFirstName: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("firstName")
  val pathUAIndividualMemberDetailsLastName: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("lastName")
  val pathUAIndividualMemberDetailsNino: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("lastName")
  val pathUAIndividualMemberDetailsSignedMandate: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("signedMandate")
  val pathUAIndividualMemberDetailsPmtMoreThan25PerFundValue: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("pmtMoreThan25PerFundValue")
  val pathUAIndividualMemberDetailsSchemePayingSurcharge: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("schemePayingSurcharge")


  /* ETMP */
  val pathEtmpEvent1Details: JsPath = __ \ "event1Details"

  // ETMP - relative paths from "event1Details"
  val pathEtmpMemberType: JsPath = __ \ Symbol("memberType")
  val pathEtmpIndividualMemberDetailsFirstName: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("firstName")
  val pathEtmpIndividualMemberDetailsLastName: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("lastName")
  val pathEtmpIndividualMemberDetailsNino: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("lastName")
  val pathEtmpIndividualMemberDetailsSignedMandate: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("signedMandate")
  val pathEtmpIndividualMemberDetailsPmtMoreThan25PerFundValue: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("pmtMoreThan25PerFundValue")
  val pathEtmpIndividualMemberDetailsSchemePayingSurcharge: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("schemePayingSurcharge")
}

