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

object API1833 {

  import EventOneReportPaths._
  import API1833ReadsUtilities._

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

private object API1833ReadsUtilities extends Transformer {

  import EventOneReportPaths._

  val readsMemberType: Reads[JsObject] = reqReadsStrTransform(pathUAWhoReceivedUnauthPayment, pathEtmpMemberType, memberTypeTransform)

  val readsIndividualOrEmployerMemberDetails: Reads[JsObject] = pathEtmpMemberType.json.pick.flatMap {
    case JsString("Individual") => readsIndividualMemberDetails
    case JsString("Employer") => readsEmployerMemberDetails
    case _ => fail[JsObject]
  }

  val readsIndividualMemberDetails: Reads[JsObject] = (
      reqReads(pathUAMembersDetailsFirstName, pathEtmpIndividualMemberDetailsFirstName) and
      reqReads(pathUAMembersDetailsLastName,pathEtmpIndividualMemberDetailsLastName) and
      reqReads(pathUAMembersDetailsNino, pathEtmpIndividualMemberDetailsNino) and
      optReadsBoolTransform(pathUADoYouHoldSignedMandate, pathEtmpIndividualMemberDetailsSignedMandate, yesNoTransform) and
      optReadsBoolTransform(pathUAValueOfUnauthorisedPayment, pathEtmpIndividualMemberDetailsPmtMoreThan25PerFundValue, yesNoTransform) and
      optReadsBoolTransform(pathUASchemeUnAuthPaySurchargeMember, pathEtmpIndividualMemberDetailsSchemePayingSurcharge, yesNoTransform)
    ).reduce

  val readsEmployerMemberDetails: Reads[JsObject] = (
      reqReads(pathUACompanyName, pathEtmpEmployerMemberDetailsCompOrOrgName) and
      reqReads(pathUACompanyNumber, pathEtmpEmployerMemberDetailsCrnNumber) and
      reqReads(pathUAEmployerAddress, pathEtmpEmployerMemberDetailsAddressDetails)
    ).reduce

  val readsUnAuthorisedPaymentDetails: Reads[JsObject] = (
      readsUnAuthorisedPmtType1WithDynamicUAPaths and
      reqReads(pathUAUnAuthorisedPaymentDate, pathEtmpUnAuthorisedPaymentDetailsDateOfUnauthorisedPayment) and
      reqReads(pathUAUnAuthorisedPaymentValue, pathEtmpUnAuthorisedPaymentDetailsValueOfUnauthorisedPayment) and
      optReadsDynamicPathStrTransform(dynamicPathUnAuthorisedPmtType2, pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType2, unAuthorisedPmtType2Transform) and
      readsFreeTxtOrSchemeOrRecipientNameWithDynamicUAPaths and
      optReads(pathUAUnAuthorisedPaymentDetailsSchemeDetailsReference, pathEtmpUnAuthorisedPaymentDetailsPstrOrReference) and
      optReads(pathUAUnAuthorisedLoanAmount, pathEtmpUnAuthorisedPaymentDetailsPmtAmtOrLoanAmt) and
      optReads(pathUAUnAuthorisedFundValue, pathEtmpUnAuthorisedPaymentDetailsFundValue) and
      readsResPropDetailsWithDynamicUAPaths
    ).reduce

  /**
   * These are utility functions for reading json from a given etmpPath into a given uaPath.
   * They may also include transformations of the etmp json before storing in the uaPath or doNothing if the node is missing in the response (i.e., it's an optional node).
   */

  lazy val reqReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick)

  lazy val optReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick).orElse(doNothing)

  lazy val reqReadsStrTransform: (JsPath, JsPath, String => String) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => String) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsString(transform(str)))
        case _ => fail[JsString]
    })
  }

  lazy val optReadsBoolTransform: (JsPath, JsPath, String => Boolean) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => Boolean) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsBoolean(transform(str)))
        case _ => fail[JsBoolean]
      }).orElse(doNothing)
    }

  lazy val optReadsDynamicPathStrTransform: (String => JsPath, JsPath, String => String) => Reads[JsObject] =
    (dynamicUaPath: String => JsPath, etmpPath: JsPath, transform: String => String) => {
      etmpPath.json.pick.flatMap {
        case JsString(str) => dynamicUaPath(transform(str)).json.copyFrom(Reads.pure(JsString(transform(str))))
        case _ => fail[JsObject]
      }.orElse(doNothing)
    }

  /**
   * These are the transforms which are applied to some of the fields.
   */

  lazy val memberTypeTransform: String => String = {
    case "Individual" => "member"
    case "Employer" => "employer"
  }

  lazy val yesNoTransform: String => Boolean = {
    case "Yes" => true
    case "No" => false
  }

  val unAuthorisedPmtType1IndividualTransform: String => String = {
    case "Benefit in kind" => "benefitInKind"
    case "Transfer to non-registered pensions scheme" => "transferToNonRegPensionScheme"
    case "Error in calculating tax free lump sums" => "errorCalcTaxFreeLumpSums"
    case "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum" => "benefitsPaidEarly"
    case "Refund of contributions" => "refundOfContributions"
    case "Overpayment of pension/written off" => "overpaymentOrWriteOff"
    case "Loans to or in respect of the employer exceeding 50% of the value of the fund" => "loansExceeding50PercentOfFundValue"
    case "Residential property held directly or indirectly by an investment-regulated pension scheme" => "residentialPropertyHeld"
    case "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme" => "tangibleMoveablePropertyHeld"
    case "Court Order Payment/Confiscation Order" => "courtOrConfiscationOrder"
    case "Other" => "memberOther"
  }

  val unAuthorisedPmtType1EmployerTransform: String => String = {
    case "Benefit in kind" => "benefitInKind"
    case "Transfer to non-registered pensions scheme" => "transferToNonRegPensionScheme"
    case "Error in calculating tax free lump sums" => "errorCalcTaxFreeLumpSums"
    case "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum" => "benefitsPaidEarly"
    case "Refund of contributions" => "refundOfContributions"
    case "Overpayment of pension/written off" => "overpaymentOrWriteOff"
    case "Loans to or in respect of the employer exceeding 50% of the value of the fund" => "loansExceeding50PercentOfFundValue"
    case "Residential property held directly or indirectly by an investment-regulated pension scheme" => "residentialProperty"
    case "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme" => "tangibleMoveableProperty"
    case "Court Order Payment/Confiscation Order" => "courtOrder"
    case "Other" => "employerOther"
  }

  lazy val unAuthorisedPmtType2Transform: String => String = {
    case "Transfer to an Employer Financed retirement Benefit scheme (EFRB)" => "anEmployerFinanced"
    case "Transfer to a non-recognised pension scheme which is not a qualifying overseas pension scheme" => "nonRecognisedScheme"
    case "Widow and/or orphan" => "widowOrOrphan"
    case "Refund of contributions other" => "other" // TODO: check what this option fills in FE Mongo, there is no equivalent transformation in API1827.scala. Consider renaming in FE: "refundOfContributionsOther"
    case "Death of member" => "deathOfMember"
    case "Death of dependent" => "deathOfDependent"
    case "Dependent no longer qualified for pension" => "dependentNoLongerQualifiedForPension"
    case "Overpayment of pension/written off other" => "other"
    case _ => ""
  }

  val freeTxtOrSchemeOrRecipientNameIndividualTransform: String => String = {
    case "Benefit in kind" => "benefitInKind"
    case "Transfer to non-registered pensions scheme" => "transferToNonRegPensionScheme"
    case "Error in calculating tax free lump sums" => "errorCalcTaxFreeLumpSums"
    case "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum" => "benefitsPaidEarly"
    case "Refund of contributions" => "refundOfContributions"
    case "Overpayment of pension/written off" => "overpaymentOrWriteOff"
    case "Loans to or in respect of the employer exceeding 50% of the value of the fund" => "loansExceeding50PercentOfFundValue"
    case "Residential property held directly or indirectly by an investment-regulated pension scheme" => "residentialPropertyHeld"
    case "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme" => "tangibleMoveablePropertyHeld"
    case "Court Order Payment/Confiscation Order" => "courtOrConfiscationOrder"
    case "Other" => "memberOther"
  }

  /**
   * These are dynamic path functions which are required for some fields as the appropriate uaPath is different for Individual and Employer.
   */

  val dynamicPathFreeTxtIndividual: String => JsPath = {
    case "Benefit in kind" => pathUABenefitInKindBriefDescription
    case "Transfer to non-registered pensions scheme" => pathUASchemeDetailsSchemeName
    case "Error in calculating tax free lump sums" => pathUAErrorDescription
    case "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum" => pathUABenefitsPaidEarly
    case "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme" => pathUAMemberTangibleMoveableProperty
    case "Court Order Payment/Confiscation Order" => pathUAUnauthorisedPaymentRecipientName
    case "Other" => pathUAMemberPaymentNatureDescription
  }

  val dynamicPathFreeTxtEmployer: String => JsPath = {
    case "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme" => pathUAEmployerTangibleMoveableProperty
    case "Court Order Payment/Confiscation Order" => pathUAUnauthorisedPaymentRecipientName
    case "Other" => pathUAPaymentNatureDesc
  }

  // TODO: refactor
  lazy val dynamicPathUnAuthorisedPmtType2: String => JsPath = {
    case "anEmployerFinanced" => pathUAWhoWasTheTransferMade
    case "nonRecognisedScheme" => pathUAWhoWasTheTransferMade
    case "widowOrOrphan" => pathUARefundOfContributions
    case "deathOfMember" => pathUAReasonForTheOverpaymentOrWriteOff
    case "deathOfDependent" => pathUAReasonForTheOverpaymentOrWriteOff
    case "dependentNoLongerQualifiedForPension" => pathUAReasonForTheOverpaymentOrWriteOff
    case "other" => pathUAReasonForTheOverpaymentOrWriteOff
  }

  /**
   * These are the most complex reads in the file because they require significant changes before being stored in a given uaPath.
   */

  private lazy val readsUnAuthorisedPmtType1IndividualOrEmployer: Reads[Option[Reads[JsObject]]] = pathEtmpMemberType.readNullable[JsString].map {
    case Some(JsString("Individual")) =>
      Some(reqReadsStrTransform(pathUAPaymentNatureMember, pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType1, unAuthorisedPmtType1IndividualTransform))
    case Some(JsString("Employer")) =>
      Some(reqReadsStrTransform(pathUAPaymentNatureEmployer, pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType1, unAuthorisedPmtType1EmployerTransform))
    case _ => None
  }

  lazy val readsUnAuthorisedPmtType1WithDynamicUAPaths: Reads[JsObject] = {
    readsUnAuthorisedPmtType1IndividualOrEmployer.flatMap(_.getOrElse(Reads.pure(Json.obj())))
  }

  private lazy val readsFreeTxtOrSchemeOrRecipientName: Reads[Option[Reads[Reads[JsObject]]]] = pathEtmpMemberType.readNullable[JsString].map { optJson =>
    optJson.map {
      case JsString("Individual") =>
        pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType1.readNullable[JsString].map {
          case Some(paymentNature) =>
            dynamicPathFreeTxtIndividual(paymentNature.value).json.copyFrom(pathEtmpUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName.json.pick).orElse(doNothing)
        }
      case JsString("Employer") =>
        pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType1.readNullable[JsString].map {
          case Some(paymentNature) => dynamicPathFreeTxtEmployer(paymentNature.value).json.copyFrom(pathEtmpUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName.json.pick).orElse(doNothing)
        }
    }
  }

  lazy val readsFreeTxtOrSchemeOrRecipientNameWithDynamicUAPaths: Reads[JsObject] = {
    readsFreeTxtOrSchemeOrRecipientName.flatMap {
      case Some(reads) => reads.flatMap(identity)
      case None => Reads.pure(Json.obj())
    }
  }

  private lazy val readsResPropDetails: Reads[Option[Reads[JsObject]]] = pathEtmpMemberType.readNullable[JsString].map {
    case Some(JsString("Individual")) => Some(optReads(pathUAMemberResidentialAddress, pathEtmpUnAuthorisedPaymentDetailsResidentialPropertyAddress))
    case Some(JsString("Employer")) => Some(optReads(pathUAEmployerResidentialAddress, pathEtmpUnAuthorisedPaymentDetailsResidentialPropertyAddress))
    case _ => None
  }

  lazy val readsResPropDetailsWithDynamicUAPaths: Reads[JsObject] = {
    readsResPropDetails.flatMap(_.getOrElse(Reads.pure(Json.obj())))
  }
}

private object EventOneReportPaths {

  /* UserAnswers */
  val pathUAEvent1MembersOrEmployers: JsPath = __ \ Symbol("event1") \ Symbol("membersOrEmployers")
  val pathUAWhoReceivedUnauthPayment: JsPath = __ \ Symbol("whoReceivedUnauthPayment")
  val pathUAMembersDetailsFirstName: JsPath = __ \ Symbol("membersDetails") \ Symbol("firstName")
  val pathUAMembersDetailsLastName: JsPath = __ \ Symbol("membersDetails")  \ Symbol("lastName")
  val pathUAMembersDetailsNino: JsPath = __ \ Symbol("membersDetails")  \ Symbol("nino")
  val pathUADoYouHoldSignedMandate: JsPath = __ \ Symbol("doYouHoldSignedMandate")
  val pathUAValueOfUnauthorisedPayment: JsPath = __ \ Symbol("valueOfUnauthorisedPayment")
  val pathUASchemeUnAuthPaySurchargeMember: JsPath = __ \ Symbol("schemeUnAuthPaySurchargeMember")
  val pathUACompanyName: JsPath = __ \ Symbol("companyDetails") \ Symbol("companyName")
  val pathUACompanyNumber: JsPath = __ \ Symbol("companyDetails") \ Symbol("companyNumber")
  val pathUAEmployerAddress: JsPath = __ \ Symbol("employerAddress")
  val pathUAUnAuthorisedPaymentDetails: JsPath = __ \ Symbol("unAuthorisedPaymentDetails")
  val pathUAPaymentNatureMember: JsPath = __ \ Symbol("paymentNatureMember")
  val pathUAPaymentNatureEmployer: JsPath = __ \ Symbol("paymentNatureEmployer")
  val pathUAUnAuthorisedPaymentDetailsSchemeDetailsReference: JsPath = __ \ Symbol("schemeDetails") \ Symbol("reference")
  val pathUAUnAuthorisedPaymentValue: JsPath = __ \ Symbol("paymentValueAndDate") \ Symbol("paymentValue")
  val pathUAUnAuthorisedPaymentDate: JsPath = __ \ Symbol("paymentValueAndDate") \ Symbol("paymentDate")
  val pathUAUnAuthorisedLoanAmount: JsPath = __ \ Symbol("loanDetails") \ Symbol("loanAmount")
  val pathUAUnAuthorisedFundValue: JsPath = __ \ Symbol("loanDetails") \ Symbol("fundValue")
  val pathUABenefitInKindBriefDescription: JsPath = __ \ Symbol("benefitInKindBriefDescription")
  val pathUASchemeDetailsSchemeName: JsPath = __ \ Symbol("schemeDetails") \ Symbol("schemeName")
  val pathUAErrorDescription: JsPath = __ \ Symbol("errorDescription")
  val pathUABenefitsPaidEarly: JsPath = __ \ Symbol("benefitsPaidEarly")
  val pathUAMemberTangibleMoveableProperty: JsPath = __ \ Symbol("memberTangibleMoveableProperty")
  val pathUAUnauthorisedPaymentRecipientName: JsPath = __ \ Symbol("unauthorisedPaymentRecipientName")
  val pathUAMemberPaymentNatureDescription: JsPath = __ \ Symbol("memberPaymentNatureDescription")
  val pathUAEmployerTangibleMoveableProperty: JsPath = __ \ Symbol("employerTangibleMoveableProperty")
  val pathUAPaymentNatureDesc: JsPath = __ \ Symbol("paymentNatureDesc")
  val pathUAWhoWasTheTransferMade: JsPath = __ \ Symbol("whoWasTheTransferMade")
  val pathUARefundOfContributions: JsPath = __ \ Symbol("refundOfContributions")
  val pathUAReasonForTheOverpaymentOrWriteOff: JsPath = __ \ Symbol("reasonForTheOverpaymentOrWriteOff")
  val pathUAMemberResidentialAddress: JsPath = __ \ Symbol("memberResidentialAddress")
  val pathUAEmployerResidentialAddress: JsPath = __ \ Symbol("employerResidentialAddress")

  /* ETMP */
  val pathEtmpEvent1Details: JsPath = __ \ "event1Details"
  val pathEtmpMemberType: JsPath = __ \ Symbol("memberType")
  val pathEtmpIndividualMemberDetailsFirstName: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("firstName")
  val pathEtmpIndividualMemberDetailsLastName: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("lastName")
  val pathEtmpIndividualMemberDetailsNino: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("nino")
  val pathEtmpIndividualMemberDetailsSignedMandate: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("signedMandate")
  val pathEtmpIndividualMemberDetailsPmtMoreThan25PerFundValue: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("pmtMoreThan25PerFundValue")
  val pathEtmpIndividualMemberDetailsSchemePayingSurcharge: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("schemePayingSurcharge")
  val pathEtmpEmployerMemberDetailsCompOrOrgName: JsPath = __ \ Symbol("employerMemberDetails") \ Symbol("compOrOrgName")
  val pathEtmpEmployerMemberDetailsCrnNumber: JsPath = __ \ Symbol("employerMemberDetails") \ Symbol("crnNumber")
  val pathEtmpEmployerMemberDetailsAddressDetails: JsPath = __ \ Symbol("employerMemberDetails") \ Symbol("addressDetails")
  val pathEtmpUnAuthorisedPaymentDetails: JsPath = __ \ Symbol("unAuthorisedPaymentDetails")
  val pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType1: JsPath = pathEtmpUnAuthorisedPaymentDetails \ Symbol("unAuthorisedPmtType1")
  val pathEtmpUnAuthorisedPaymentDetailsUnAuthorisedPmtType2: JsPath = pathEtmpUnAuthorisedPaymentDetails \ Symbol("unAuthorisedPmtType2")
  val pathEtmpUnAuthorisedPaymentDetailsFreeTxtOrSchemeOrRecipientName: JsPath = pathEtmpUnAuthorisedPaymentDetails \ Symbol("freeTxtOrSchemeOrRecipientName")
  val pathEtmpUnAuthorisedPaymentDetailsPstrOrReference: JsPath = pathEtmpUnAuthorisedPaymentDetails \ Symbol("pstrOrReference")
  val pathEtmpUnAuthorisedPaymentDetailsDateOfUnauthorisedPayment: JsPath = pathEtmpUnAuthorisedPaymentDetails \ Symbol("dateOfUnauthorisedPayment")
  val pathEtmpUnAuthorisedPaymentDetailsValueOfUnauthorisedPayment: JsPath = pathEtmpUnAuthorisedPaymentDetails \ Symbol("valueOfUnauthorisedPayment")
  val pathEtmpUnAuthorisedPaymentDetailsPmtAmtOrLoanAmt: JsPath = pathEtmpUnAuthorisedPaymentDetails \ Symbol("pmtAmtOrLoanAmt")
  val pathEtmpUnAuthorisedPaymentDetailsFundValue: JsPath = pathEtmpUnAuthorisedPaymentDetails \ Symbol("fundValue")
  val pathEtmpUnAuthorisedPaymentDetailsResidentialPropertyAddress: JsPath = pathEtmpUnAuthorisedPaymentDetails \ Symbol("residentialPropertyAddress")
}
