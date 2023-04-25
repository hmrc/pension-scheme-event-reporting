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

  private lazy val readsEvent1Details: Reads[JsArray] = __.read(
    Reads.seq(
      (
        readsMemberType and
        readsIndividualOrEmployerMemberDetails and
        readsUnAuthorisedPaymentDetails
      ).reduce
    ).map(JsArray(_)))
}

private object EventOneReportReadsUtilities extends Transformer {

  import EventOneReportPaths._

  lazy val reqReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick)

  lazy val optReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick).orElse(doNothing)

  lazy val reqReadsWithStringTransform: (JsPath, JsPath, String => String) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => String) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsString(transform(str)))
        case _ => fail[JsString]
    })
  }

  lazy val optReadsWithBooleanTransform: (JsPath, JsPath, String => Boolean) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => Boolean) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsBoolean(transform(str)))
        case _ => fail[JsBoolean]
      }).orElse(doNothing)
    }

  lazy val optReadsWithStringTransform: (JsPath, JsPath, String => String) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => String) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsString(transform(str)))
        case _ => fail[JsString]
      }).orElse(doNothing)
    }

  val readsMemberType: Reads[JsObject] = reqReadsWithStringTransform(pathUAWhoReceivedUnauthPayment, pathEtmpMemberType, memberTypeTransform)

  lazy val memberTypeTransform: String => String = {
    case "Individual" => "member"
    case "Employer" => "employer"
  }

  val readsIndividualOrEmployerMemberDetails: Reads[JsObject] = pathEtmpMemberType.json.pick.flatMap {
    case JsString("Individual") => readsIndividualMemberDetails
    case JsString("Employer") => readsEmployerMemberDetails
    case _ => fail[JsObject]
  }

  lazy val yesNoTransform: String => Boolean = {
    case "Yes" => true
    case "No" => false
  }

  val readsIndividualMemberDetails: Reads[JsObject] = (
    reqReads(pathUAMembersDetailsFirstName, pathEtmpIndividualMemberDetailsFirstName) and
      reqReads(pathUAMembersDetailsLastName,pathEtmpIndividualMemberDetailsLastName) and
      reqReads(pathUAMembersDetailsNino, pathEtmpIndividualMemberDetailsNino) and
      optReadsWithBooleanTransform(pathUADoYouHoldSignedMandate, pathEtmpIndividualMemberDetailsSignedMandate, yesNoTransform) and
      optReadsWithBooleanTransform(pathUAValueOfUnauthorisedPayment, pathEtmpIndividualMemberDetailsPmtMoreThan25PerFundValue, yesNoTransform) and
      optReadsWithBooleanTransform(pathUASchemeUnAuthPaySurchargeMember, pathEtmpIndividualMemberDetailsSchemePayingSurcharge, yesNoTransform)
    ).reduce

  val readsEmployerMemberDetails: Reads[JsObject] = (
    reqReads(pathUACompanyName, pathEtmpEmployerMemberDetailsCompOrOrgName) and
    reqReads(pathUACompanyNumber, pathEtmpEmployerMemberDetailsCrnNumber) and
      // TODO: Consider trying to use with defined transform? // val readsAddressFn: JsPath => Reads[JsObject] = readsAddress
    pathUAEmployerAddress.json.copyFrom(readsAddress(pathEtmpEmployerMemberDetailsAddressDetails))
    ).reduce

  val readsUnAuthorisedPaymentDetails: Reads[JsObject] = (

      // TODO: dynamic UA path required for pmtType1.
      pathUAUnAuthorisedPaymentDetailsUnAuthorisedPmtType1.json.copyFrom(pathETMPUnAuthorisedPaymentDetailsUnAuthorisedPmtType1.json.pick) and

      reqReads(pathUAUnAuthorisedPaymentDate, pathETMPUnAuthorisedPaymentDetailsDateOfUnauthorisedPayment) and
      reqReads(pathUAUnAuthorisedPaymentValue, pathETMPUnAuthorisedPaymentDetailsValueOfUnauthorisedPayment) and

      // TODO: dynamic UA path required for pmtType2.
      optReads(pathUAUnAuthorisedPaymentDetailsUnAuthorisedPmtType2, pathETMPUnAuthorisedPaymentDetailsUnAuthorisedPmtType2) and

      // TODO: dynamic UA path required for freeTxtOrSchemeOrRecipientName.
      optReads(pathUAUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName, pathETMPUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName) and

      optReads(pathUAUnAuthorisedPaymentDetailsSchemeDetailsReference, pathETMPUnAuthorisedPaymentDetailsPstrOrReference) and
      optReads(pathUAUnAuthorisedLoanAmount, pathETMPUnAuthorisedPaymentDetailsPmtAmtOrLoanAmt) and
      optReads(pathUAUnAuthorisedFundValue, pathETMPUnAuthorisedPaymentDetailsFundValue) and

      // TODO: dynamic UA path required for resPropAdd.
      optReads(pathUAUnAuthorisedPaymentDetailsResidentialPropertyAddress, pathUAUnAuthorisedPaymentDetailsResidentialPropertyAddress)
    ).reduce
}

private object EventOneReportPaths {

  /*
  UserAnswers
  */
  val pathUAEvent1MembersOrEmployers: JsPath = __ \ Symbol("event1") \ Symbol("membersOrEmployers")
  // TODO: these will need to be amended to match the actual UA structure.
  // Member type
  val pathUAWhoReceivedUnauthPayment: JsPath = __ \ Symbol("whoReceivedUnauthPayment")
  // Individual
  val pathUAMembersDetailsFirstName: JsPath = __ \ Symbol("membersDetails") \ Symbol("firstName")
  val pathUAMembersDetailsLastName: JsPath = __ \ Symbol("membersDetails")  \ Symbol("lastName")
  val pathUAMembersDetailsNino: JsPath = __ \ Symbol("membersDetails")  \ Symbol("nino")
  val pathUADoYouHoldSignedMandate: JsPath = __ \ Symbol("doYouHoldSignedMandate")
  val pathUAValueOfUnauthorisedPayment: JsPath = __ \ Symbol("valueOfUnauthorisedPayment")
  val pathUASchemeUnAuthPaySurchargeMember: JsPath = __ \ Symbol("schemeUnAuthPaySurchargeMember")
  // Employer
  // TODO: check this is correct by completing 1 Employer journey and inspecting Mongo, looks odd / too nested in event1 again?
  val pathUACompanyName: JsPath = __ \ Symbol("event1") \ Symbol("companyDetails") \ Symbol("companyName")
  val pathUACompanyNumber: JsPath = __ \ Symbol("event1") \ Symbol("companyDetails") \ Symbol("companyNumber")
  val pathUAEmployerAddress: JsPath = __ \ Symbol("employerAddress")
  // Unauthorised payment details
  val pathUAUnAuthorisedPaymentDetails: JsPath = __ \ Symbol("unAuthorisedPaymentDetails")
  // TODO: should be dynamic.
  val pathUAUnAuthorisedPaymentDetailsUnAuthorisedPmtType1: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("unAuthorisedPmtType1")

  // TODO: should be dynamic.
  val pathUAUnAuthorisedPaymentDetailsUnAuthorisedPmtType2: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("unAuthorisedPmtType2")

  // TODO: should be dynamic.
  val pathUAUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName: JsPath = pathUAUnAuthorisedPaymentDetails \ Symbol("freeTxtOrSchemeOrRecipientName")

  val pathUAUnAuthorisedPaymentDetailsSchemeDetailsReference: JsPath = __ \ Symbol("schemeDetails") \ Symbol("reference")
  val pathUAUnAuthorisedPaymentValue: JsPath = __ \ Symbol("paymentValueAndDate") \ Symbol("paymentValue")
  val pathUAUnAuthorisedPaymentDate: JsPath = __ \ Symbol("paymentValueAndDate") \ Symbol("paymentDate")
  val pathUAUnAuthorisedLoanAmount: JsPath = __ \ Symbol("loanDetails") \ Symbol("loanAmount")
  val pathUAUnAuthorisedFundValue: JsPath = __ \ Symbol("loanDetails") \ Symbol("fundValue")

  // TODO: should be dynamic.
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

