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

  import EventOneReportPaths._
  import EventOneReportReadsUtilities._

  implicit val rds1833Api: Reads[JsObject] =
    pathUAEvent1MembersOrEmployers.json.copyFrom(pathEtmpEvent1Details.read(readsEvent1Details))

  private lazy val readsEvent1Details: Reads[JsArray] = __.read(Reads.seq((
      readsMemberType and readsIndividualOrEmployerMemberDetails and readsUnAuthorisedPaymentDetails
    ).reduce).map(JsArray(_)))
}

private object EventOneReportReadsUtilities extends Transformer {

  import EventOneReportPaths._

  lazy val requiredReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => {
    uaPath.json.copyFrom(etmpPath.json.pick)
  }

  lazy val requiredReadsWithTransform: (JsPath, JsPath, String => String) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => String) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsString(transform(str)))
        case _ => fail[JsString]
    })
  }

  lazy val optionalReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => {
    uaPath.json.copyFrom(etmpPath.json.pick).orElse(doNothing)
  }

  lazy val optionalReadsWithTransform: (JsPath, JsPath, String => String) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => String) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsString(transform(str)))
        case _ => fail[JsString]
      }).orElse(doNothing)
    }

  val readsMemberType: Reads[JsObject] = requiredReads(pathUAMemberType, pathEtmpMemberType)

  val readsIndividualOrEmployerMemberDetails: Reads[JsObject] = pathEtmpMemberType.json.pick.flatMap {
    case JsString("Individual") => readsIndividualMemberDetails
    case JsString("Employer") => readsEmployerMemberDetails
    case _ => fail[JsObject]
  }

  val readsIndividualMemberDetails: Reads[JsObject] = (
    requiredReads(pathUAIndividualMemberDetailsFirstName, pathEtmpIndividualMemberDetailsFirstName) and
      requiredReads(pathUAIndividualMemberDetailsLastName,pathEtmpIndividualMemberDetailsLastName) and
      requiredReads(pathUAIndividualMemberDetailsNino, pathEtmpIndividualMemberDetailsNino) and
      optionalReads(pathUAIndividualMemberDetailsSignedMandate, pathEtmpIndividualMemberDetailsSignedMandate) and
      optionalReads(pathUAIndividualMemberDetailsPmtMoreThan25PerFundValue, pathEtmpIndividualMemberDetailsPmtMoreThan25PerFundValue) and
      optionalReads(pathUAIndividualMemberDetailsSchemePayingSurcharge, pathEtmpIndividualMemberDetailsSchemePayingSurcharge)
    ).reduce

  val readsEmployerMemberDetails: Reads[JsObject] = (
    requiredReads(pathUAEmployerMemberDetailsCompOrOrgName, pathEtmpEmployerMemberDetailsCompOrOrgName) and
    requiredReads(pathUAEmployerMemberDetailsCrnNumber, pathEtmpEmployerMemberDetailsCrnNumber) and
      // TODO: Consider trying to use with defined transform? // val readsAddressFn: JsPath => Reads[JsObject] = readsAddress
    pathUAEmployerMemberDetailsAddressDetails.json.copyFrom(readsAddress(pathEtmpEmployerMemberDetailsAddressDetails))
    ).reduce

  val readsUnAuthorisedPaymentDetails: Reads[JsObject] = (
      requiredReads(pathUAUnAuthorisedPaymentDetailsUnAuthorisedPmtType1, pathETMPUnAuthorisedPaymentDetailsUnAuthorisedPmtType1) and
      requiredReads(pathUAUnAuthorisedPaymentDetailsDateOfUnauthorisedPayment, pathETMPUnAuthorisedPaymentDetailsDateOfUnauthorisedPayment) and
      requiredReads(pathUAUnAuthorisedPaymentDetailsValueOfUnauthorisedPayment, pathETMPUnAuthorisedPaymentDetailsValueOfUnauthorisedPayment) and
      optionalReads(pathUAUnAuthorisedPaymentDetailsUnAuthorisedPmtType2, pathETMPUnAuthorisedPaymentDetailsUnAuthorisedPmtType2) and
      optionalReads(pathUAUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName, pathETMPUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName) and
      optionalReads(pathUAUnAuthorisedPaymentDetailsPstrOrReference, pathETMPUnAuthorisedPaymentDetailsPstrOrReference) and
      optionalReads(pathUAUnAuthorisedPaymentDetailsPmtAmtOrLoanAmt, pathETMPUnAuthorisedPaymentDetailsPmtAmtOrLoanAmt) and
      optionalReads(pathUAUnAuthorisedPaymentDetailsFundValue, pathETMPUnAuthorisedPaymentDetailsFundValue) and
      optionalReads(pathUAUnAuthorisedPaymentDetailsResidentialPropertyAddress, pathUAUnAuthorisedPaymentDetailsResidentialPropertyAddress)
    ).reduce
}

private object EventOneReportPaths {

  /*
  UserAnswers
  */
  val pathUAEvent1MembersOrEmployers: JsPath = __ \ Symbol("event1") \ Symbol("membersOrEmployers")
  // TODO: these will need to be amended to match the actual UA structure.
  // Member type
  val pathUAMemberType: JsPath = __ \ Symbol("memberType")
  // Individual
  val pathUAIndividualMemberDetailsFirstName: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("firstName")
  val pathUAIndividualMemberDetailsLastName: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("lastName")
  val pathUAIndividualMemberDetailsNino: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("lastName")
  val pathUAIndividualMemberDetailsSignedMandate: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("signedMandate")
  val pathUAIndividualMemberDetailsPmtMoreThan25PerFundValue: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("pmtMoreThan25PerFundValue")
  val pathUAIndividualMemberDetailsSchemePayingSurcharge: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("schemePayingSurcharge")
  // Employer
  val pathUAEmployerMemberDetailsCompOrOrgName: JsPath = __ \ Symbol("employerMemberDetails") \ Symbol("compOrOrgName")
  val pathUAEmployerMemberDetailsCrnNumber: JsPath = __ \ Symbol("employerMemberDetails") \ Symbol("crnNumber")
  val pathUAEmployerMemberDetailsAddressDetails: JsPath = __ \ Symbol("employerMemberDetails") \ Symbol("addressDetails")
  // Unauthorised payment details
  val pathUAUnAuthorisedPaymentDetails: JsPath = __ \ Symbol("unAuthorisedPaymentDetails")
  val pathUAUnAuthorisedPaymentDetailsUnAuthorisedPmtType1: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("unAuthorisedPmtType1")
  val pathUAUnAuthorisedPaymentDetailsUnAuthorisedPmtType2: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("unAuthorisedPmtType2")
  val pathUAUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("freeTxtOrSchemeOrRecipientName")
  val pathUAUnAuthorisedPaymentDetailsPstrOrReference: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("pstrOrReference")
  val pathUAUnAuthorisedPaymentDetailsDateOfUnauthorisedPayment: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("dateOfUnauthorisedPayment")
  val pathUAUnAuthorisedPaymentDetailsValueOfUnauthorisedPayment: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("valueOfUnauthorisedPayment")

  val pathUAUnAuthorisedPaymentDetailsPmtAmtOrLoanAmt: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("pmtAmtOrLoanAmt")
  val pathUAUnAuthorisedPaymentDetailsFundValue: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("fundValue")
  val pathUAUnAuthorisedPaymentDetailsResidentialPropertyAddress: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("residentialPropertyAddress")



  /*
  ETMP
  */
  val pathEtmpEvent1Details: JsPath = __ \ "event1Details"


  // ETMP - relative paths from "event1Details"
  val pathEtmpMemberType: JsPath = __ \ Symbol("memberType")
  // Individual member details
  val pathEtmpIndividualMemberDetailsFirstName: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("firstName")
  val pathEtmpIndividualMemberDetailsLastName: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("lastName")
  val pathEtmpIndividualMemberDetailsNino: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("lastName")
  val pathEtmpIndividualMemberDetailsSignedMandate: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("signedMandate")
  val pathEtmpIndividualMemberDetailsPmtMoreThan25PerFundValue: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("pmtMoreThan25PerFundValue")
  val pathEtmpIndividualMemberDetailsSchemePayingSurcharge: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("schemePayingSurcharge")
  // Employer member details
  val pathEtmpEmployerMemberDetailsCompOrOrgName: JsPath = __ \ Symbol("employerMemberDetails") \ Symbol("compOrOrgName")
  val pathEtmpEmployerMemberDetailsCrnNumber: JsPath = __ \ Symbol("employerMemberDetails") \ Symbol("crnNumber")
  val pathEtmpEmployerMemberDetailsAddressDetails: JsPath = __ \ Symbol("employerMemberDetails") \ Symbol("addressDetails")

  // Unauthorised payment details
  val pathETMPUnAuthorisedPaymentDetails: JsPath = __ \ Symbol("unAuthorisedPaymentDetails")
  val pathETMPUnAuthorisedPaymentDetailsUnAuthorisedPmtType1: JsPath = pathETMPUnAuthorisedPaymentDetails \ Symbol("unAuthorisedPmtType1")
  val pathETMPUnAuthorisedPaymentDetailsUnAuthorisedPmtType2: JsPath = pathETMPUnAuthorisedPaymentDetails \ Symbol("unAuthorisedPmtType2")
  val pathETMPUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName: JsPath = pathETMPUnAuthorisedPaymentDetails \ Symbol("freeTxtOrSchemeOrRecipientName")
  val pathETMPUnAuthorisedPaymentDetailsPstrOrReference: JsPath = pathETMPUnAuthorisedPaymentDetails \ Symbol("pstrOrReference")
  val pathETMPUnAuthorisedPaymentDetailsDateOfUnauthorisedPayment: JsPath = pathETMPUnAuthorisedPaymentDetails \ Symbol("dateOfUnauthorisedPayment")
  val pathETMPUnAuthorisedPaymentDetailsValueOfUnauthorisedPayment: JsPath = pathETMPUnAuthorisedPaymentDetails \ Symbol("valueOfUnauthorisedPayment")
  val pathETMPUnAuthorisedPaymentDetailsPmtAmtOrLoanAmt: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("pmtAmtOrLoanAmt")
  val pathETMPUnAuthorisedPaymentDetailsFundValue: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("fundValue")
  val pathETMPUnAuthorisedPaymentDetailsResidentialPropertyAddress: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("residentialPropertyAddress")
}

