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

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.{ReadsUtils, Transformer}

object API1827 {
  import transformations.UserAnswersToETMP.API1827Paths._
  import transformations.UserAnswersToETMP.API1827ReadsUtilities._

  val transformToETMPData: Reads[JsObject] = {
    val reads = uaPathToMembersOrEmployers.readNullable[JsArray](__.read(Reads.seq(readsMember))
      .map(JsArray(_))).map {
        case None =>  Json.obj()
        case Some(x) if x.value.isEmpty => Json.obj()
        case Some(x) =>
          Json.obj(
            "event1Details" -> Json.obj(
              "event1Details" -> x
            )
          )
    }

    for {
      jsObject <- reads
      header <- HeaderForAllAPIs.transformToETMPData()
    } yield {
      header ++ jsObject
    }

  }
}

private object API1827ReadsUtilities extends Transformer with ReadsUtils {
  import transformations.UserAnswersToETMP.API1827Paths._

  private val paymentNatureTypeKeyBenefitInKind: String = "benefitInKind"
  private val paymentNatureTypeKeyTransferToNonRegPensionScheme: String = "transferToNonRegPensionScheme"
  private val paymentNatureTypeKeyRefundOfContributions: String = "refundOfContributions"
  private val paymentNatureTypeKeyOverpaymentOrWriteOff: String = "overpaymentOrWriteOff"
  private val paymentNatureTypeKeyResidentialPropertyHeld: String = "residentialPropertyHeld"
  private val paymentNatureTypeKeyResidentialPropertyHeldEmployer: String = "residentialProperty"
  private val paymentNatureTypeKeyTangibleMoveablePropertyHeld: String = "tangibleMoveablePropertyHeld"
  private val paymentNatureTypeKeyTangibleMoveablePropertyHeldEmployer: String = "tangibleMoveableProperty"
  private val paymentNatureTypeKeyErrorCalcTaxFreeLumpSums: String = "errorCalcTaxFreeLumpSums"
  private val paymentNatureTypeKeyCourtOrConfiscationOrder: String = "courtOrConfiscationOrder"
  private val paymentNatureTypeKeyCourtOrConfiscationOrderEmployer: String = "courtOrder"
  private val paymentNatureTypeKeyLoansExceeding50PercentOfFundValue: String = "loansExceeding50PercentOfFundValue"
  private val paymentNatureTypeKeyOther: String = "memberOther"
  private val paymentNatureTypeKeyOtherEmployer: String = "employerOther"
  private val paymentNatureTypeKeyBenefitsPaidEarly: String = "benefitsPaidEarly"
  private val whoReceivedUnauthPaymentIndividual = "Individual"
  private val whoReceivedUnauthPaymentEmployer = "Employer"

  private val paymentNatureMemberMap = Map(
    paymentNatureTypeKeyBenefitInKind -> "Benefit in kind",
    paymentNatureTypeKeyTransferToNonRegPensionScheme -> "Transfer to non-registered pensions scheme",
    paymentNatureTypeKeyErrorCalcTaxFreeLumpSums -> "Error in calculating tax free lump sums",
    paymentNatureTypeKeyBenefitsPaidEarly -> "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum",
    paymentNatureTypeKeyRefundOfContributions -> "Refund of contributions",
    paymentNatureTypeKeyOverpaymentOrWriteOff -> "Overpayment of pension/written off",
    paymentNatureTypeKeyResidentialPropertyHeld -> "Residential property held directly or indirectly by an investment-regulated pension scheme",
    paymentNatureTypeKeyTangibleMoveablePropertyHeld -> "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme",
    paymentNatureTypeKeyCourtOrConfiscationOrder -> "Court Order Payment/Confiscation Order",
    paymentNatureTypeKeyOther -> "Other"
  )

  private val paymentNatureEmployerMap = Map(
    paymentNatureTypeKeyLoansExceeding50PercentOfFundValue -> "Loans to or in respect of the employer exceeding 50% of the value of the fund",
    paymentNatureTypeKeyResidentialPropertyHeldEmployer -> "Residential property held directly or indirectly by an investment-regulated pension scheme",
    paymentNatureTypeKeyTangibleMoveablePropertyHeldEmployer ->
      "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme",
    paymentNatureTypeKeyCourtOrConfiscationOrderEmployer -> "Court Order Payment/Confiscation Order",
    paymentNatureTypeKeyOtherEmployer -> "Other"
  )

  private val whoWasTransferMadeToMap = Map(
    "anEmployerFinanced" -> "Transfer to an Employer Financed retirement Benefit scheme (EFRB)",
    "nonRecognisedScheme" -> "Transfer to a non-recognised pension scheme which is not a qualifying overseas pension scheme",
    "other" -> "Overpayment of pension/written off other"
  )

  private val refundOfContributionsMap = Map(
    "widowOrOrphan" -> "Widow and/or orphan",
    "other" -> "Overpayment of pension/written off other"
  )

  private val overpaymentOrWriteOffMap = Map(
    "deathOfMember" -> "Death of member",
    "deathOfDependent" -> "Death of dependent",
    "dependentNoLongerQualifiedForPension" -> "Dependent no longer qualified for pension",
    "other" -> "Overpayment of pension/written off other"
  )

  //scalastyle:off cyclomatic.complexity
  private def freeTxtOrSchemeOrRecipientName(paymentNature: String, whoReceivedUnauthorisedPayment: String): Reads[JsString] = {
    (paymentNature, whoReceivedUnauthorisedPayment) match {
      case (`paymentNatureTypeKeyBenefitInKind`, `whoReceivedUnauthPaymentIndividual`) =>
        uaPathToBenefitInKindBriefDescription.json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyTransferToNonRegPensionScheme`, `whoReceivedUnauthPaymentIndividual`) =>
        uaPathToSchemeName.json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyErrorCalcTaxFreeLumpSums`, `whoReceivedUnauthPaymentIndividual`) =>
        uaPathToErrorDescription.json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyBenefitsPaidEarly`, `whoReceivedUnauthPaymentIndividual`) =>
        uaPathToBenefitsPaidEarly.json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyTangibleMoveablePropertyHeld`, `whoReceivedUnauthPaymentIndividual`) =>
        uaPathToMemberTangibleMoveableProperty.json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyTangibleMoveablePropertyHeldEmployer`, `whoReceivedUnauthPaymentEmployer`) =>
        uaPathToEmployerTangibleMoveableProperty.json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyCourtOrConfiscationOrder`, _) =>
        uaPathToUnauthorisedPaymentRecipientName.json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyCourtOrConfiscationOrderEmployer`, _) =>
        uaPathToUnauthorisedPaymentRecipientName.json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyOther`, `whoReceivedUnauthPaymentIndividual`) =>
        uaPathToMemberPaymentNatureDescription.json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyOtherEmployer`, `whoReceivedUnauthPaymentEmployer`) =>
        uaPathToPaymentNatureDesc.json.pick.map(_.as[JsString])
      case _ => Reads[JsString](_ => JsError(""))
    }
  }

  private def pstrOrReference(paymentNature: String): Reads[JsString] = {
    paymentNature match {
      case `paymentNatureTypeKeyTransferToNonRegPensionScheme` => uaPathToReference.json.pick.map(_.as[JsString])
      case _ => Reads[JsString](_ => JsError(""))
    }
  }

  private val readsPaymentNatureMember: Reads[String] = uaPathToPaymentNatureMember.json.pick.map(_.as[JsString].value)
  private val readsPaymentNatureEmployer: Reads[String] = uaPathToPaymentNatureEmployer.json.pick.map(_.as[JsString].value)

  private val readsWhoReceivedUnauthorisedPayment: Reads[String] = {
    etmpPathToWhoReceivedUnauthPayment.json.pick.flatMap {
      case JsString("member") => Reads.pure[String](whoReceivedUnauthPaymentIndividual)
      case JsString("employer") => Reads.pure[String](whoReceivedUnauthPaymentEmployer)
      case _ => fail
    }
  }

  private val schemePayingSurcharge: Reads[JsObject] =
    etmpPathToValueOfUnauthorisedPayment.read[Boolean].flatMap {
      case true => etmpPathToSchemePayingSurcharge.json
        .copyFrom(uaPathToSchemeUnAuthPaySurchargeMember.json.pick.map(toYesNo))
      case _ => fail
    }

  private val readsIndividualMemberDetails: Reads[JsObject] =
    (reqReads(etmpPathToIndividualMemberFirstName, uaPathToIndividualMemberFirstName) and
      reqReads(etmpPathToIndividualMemberLastName, uaPathToIndividualMemberLastName) and
      reqReads(etmpPathToIndividualMemberNino, uaPathToIndividualMemberNino) and
      reqNestedReadsJsString(etmpPathToIndividualMemberSignedMandate, uaPathToDoYouHoldSignedMandate.json.pick.map(toYesNo)) and
      reqNestedReadsJsString(etmpPathToIndividualMemberPmtMoreThan25PerFundValue, uaPathToValueOfUnauthorisedPayment.json.pick.map(toYesNo)) and
      schemePayingSurcharge.orElse(doNothing)).reduce

  private val readsEmployerDetails: Reads[JsObject] =
    (reqReads(etmpPathToEmployerMemberCompOrOrgName, uaPathToCompanyName) and
      reqReads(etmpPathToEmployerMemberCrnNumber, uaPathToCompanyNumber) and
      etmpPathToEmployerMemberAddressDetails.json.copyFrom(readsAddress(uaPathToCompanyAddress)).orElse(doNothing)
      ).reduce

  private def loanPymtPgs(paymentNature: String): Reads[JsObject] = {
    def readsLoanAmount: Reads[JsObject] = reqReads(etmpPathToPmtAmtOrLoanAmt, uaPathToLoanAmount)

    def readsFundValue: Reads[JsObject] = reqReads(etmpPathToFundValue, uaPathToFundValue)

    paymentNature match {
      case `paymentNatureTypeKeyLoansExceeding50PercentOfFundValue` => (readsLoanAmount and readsFundValue).reduce
      case `paymentNatureTypeKeyCourtOrConfiscationOrderEmployer` => readsLoanAmount
      case _ => fail
    }
  }

  private def readsPaymentType2(paymentNature: String): Reads[JsString] = paymentNature match {
    case `paymentNatureTypeKeyTransferToNonRegPensionScheme` =>
      uaPathToWhoWasTheTransferMade.json.pick.map(jsValue => JsString(whoWasTransferMadeToMap(jsValue.as[JsString].value)))
    case `paymentNatureTypeKeyRefundOfContributions` =>
      uaPathToRefundOfContributions.json.pick.map(jsValue => JsString(refundOfContributionsMap(jsValue.as[JsString].value)))
    case `paymentNatureTypeKeyOverpaymentOrWriteOff` =>
      uaPathToReasonForTheOverpaymentOrWriteOff.json.pick.map(jsValue => JsString(overpaymentOrWriteOffMap(jsValue.as[JsString].value)))
    case _ => fail
  }

  private def readsUnauthorisedPaymentDetails(paymentNature: String, whoReceivedUnauthorisedPayment: String): Reads[JsObject] = {
    val readsResidentialAddressMember: Reads[JsObject] = paymentNature match {
      case `paymentNatureTypeKeyResidentialPropertyHeld` => readsAddress(uaPathToMemberResidentialAddress)
      case _ => fail
    }

    val readsResidentialAddressEmployer: Reads[JsObject] = paymentNature match {
      case `paymentNatureTypeKeyResidentialPropertyHeldEmployer` => readsAddress(uaPathToEmployerResidentialAddress)
      case _ => fail
    }

    whoReceivedUnauthorisedPayment match {
      case `whoReceivedUnauthPaymentIndividual` =>
        (etmpPathToUnAuthorisedPmtType1.json.put(JsString(paymentNatureMemberMap(paymentNature))) and
          optNestedReadsJsString(etmpPathToFreeTxtOrSchemeOrRecipientName, freeTxtOrSchemeOrRecipientName(paymentNature, whoReceivedUnauthorisedPayment)) and
          optNestedReadsJsString(etmpPathToPstrOrReference, pstrOrReference(paymentNature)) and
          optNestedReadsJsString(etmpPathToUnAuthorisedPmtType2, readsPaymentType2(paymentNature)) and
          reqReads(etmpPathToValueOfUnauthorisedPaymentFromDetails, uaPathToPaymentValue) and
          reqReads(etmpPathToDateOfUnauthorisedPayment, uaPathToPaymentDate) and
          optNestedReadsJsObject(etmpPathToResidentialPropertyAddress, readsResidentialAddressMember)
          ).reduce
      case `whoReceivedUnauthPaymentEmployer` =>
        (etmpPathToUnAuthorisedPmtType1.json.put(JsString(paymentNatureEmployerMap(paymentNature))) and
          optNestedReadsJsString(etmpPathToFreeTxtOrSchemeOrRecipientName, freeTxtOrSchemeOrRecipientName(paymentNature, whoReceivedUnauthorisedPayment)) and
          loanPymtPgs(paymentNature).orElse(doNothing) and
          optNestedReadsJsObject(etmpPathToResidentialPropertyAddress, readsResidentialAddressEmployer) and
          reqReads(etmpPathToValueOfUnauthorisedPaymentFromDetails, uaPathToPaymentValue) and
          reqReads(etmpPathToDateOfUnauthorisedPayment, uaPathToPaymentDate)
          ).reduce
      case _ => fail
    }
  }

  private def readsMemberOrEmployer(whoReceivedUnauthorisedPayment: String): Reads[JsObject] = {
    whoReceivedUnauthorisedPayment match {
      case `whoReceivedUnauthPaymentIndividual` => readsIndividualMemberDetails
      case `whoReceivedUnauthPaymentEmployer` => readsEmployerDetails
      case _ => fail
    }
  }

  val readsMember: Reads[JsObject] = {
    (for {
      whoReceivedUnauthorisedPayment <- readsWhoReceivedUnauthorisedPayment
      paymentNature <- whoReceivedUnauthorisedPayment match {
        case `whoReceivedUnauthPaymentIndividual` => readsPaymentNatureMember
        case `whoReceivedUnauthPaymentEmployer` => readsPaymentNatureEmployer
        case _ => fail
      }
    } yield {
      (
        etmpPathToMemberType.json.put(JsString(whoReceivedUnauthorisedPayment)) and
          reqReads(etmpPathToMemberStatus, uaPathToMemberStatus) and
          optReads(etmpPathToAmendedVersion, uaPathToAmendedVersion) and
          readsMemberOrEmployer(whoReceivedUnauthorisedPayment) and
          readsUnauthorisedPaymentDetails(paymentNature, whoReceivedUnauthorisedPayment)
        ).reduce
    }).flatMap[JsObject](identity)
  }

}

private object API1827Paths {
  // ETMP
  val etmpPathToEmployerMemberDetails:                      JsPath = __ \ "employerMemDetails"
  val etmpPathToEmployerMemberCompOrOrgName:                JsPath = etmpPathToEmployerMemberDetails \ "compOrOrgName"
  val etmpPathToEmployerMemberCrnNumber:                    JsPath = etmpPathToEmployerMemberDetails \ "crnNumber"
  val etmpPathToEmployerMemberAddressDetails:               JsPath = etmpPathToEmployerMemberDetails \ "addressDetails"

  val etmpPathToIndividualMemberDetails:                    JsPath = __ \ "individualMemberDetails"
  val etmpPathToIndividualMemberFirstName:                  JsPath = etmpPathToIndividualMemberDetails \ "firstName"
  val etmpPathToIndividualMemberLastName:                   JsPath = etmpPathToIndividualMemberDetails \ "lastName"
  val etmpPathToIndividualMemberNino:                       JsPath = etmpPathToIndividualMemberDetails \ "nino"
  val etmpPathToIndividualMemberSignedMandate:              JsPath = etmpPathToIndividualMemberDetails \ "signedMandate"
  val etmpPathToIndividualMemberPmtMoreThan25PerFundValue:  JsPath = etmpPathToIndividualMemberDetails \ "pmtMoreThan25PerFundValue"
  val etmpPathToSchemePayingSurcharge:                      JsPath = etmpPathToIndividualMemberDetails \ "schemePayingSurcharge"

  val etmpPathToAmendedVersion:                             JsPath = __ \ "amendedVersion"
  private val etmpPathToUnauthorisedPaymentDetails:         JsPath = __ \ "unAuthorisedPaymentDetails"
  val etmpPathToDateOfUnauthorisedPayment:                  JsPath = etmpPathToUnauthorisedPaymentDetails \ "dateOfUnauthorisedPayment"
  val etmpPathToFundValue:                                  JsPath = etmpPathToUnauthorisedPaymentDetails \ "fundValue"
  val etmpPathToFreeTxtOrSchemeOrRecipientName:             JsPath = etmpPathToUnauthorisedPaymentDetails \ "freeTxtOrSchemeOrRecipientName"
  val etmpPathToPmtAmtOrLoanAmt:                            JsPath = etmpPathToUnauthorisedPaymentDetails \ "pmtAmtOrLoanAmt"
  val etmpPathToPstrOrReference:                            JsPath = etmpPathToUnauthorisedPaymentDetails \ "pstrOrReference"
  val etmpPathToResidentialPropertyAddress:                 JsPath = etmpPathToUnauthorisedPaymentDetails \ "residentialPropertyAddress"
  val etmpPathToUnAuthorisedPmtType2:                       JsPath = etmpPathToUnauthorisedPaymentDetails \ "unAuthorisedPmtType2"
  val etmpPathToUnAuthorisedPmtType1:                       JsPath = etmpPathToUnauthorisedPaymentDetails \ "unAuthorisedPmtType1"
  val etmpPathToValueOfUnauthorisedPaymentFromDetails:      JsPath = etmpPathToUnauthorisedPaymentDetails \ "valueOfUnauthorisedPayment"

  val etmpPathToMemberStatus:                               JsPath = __ \ "memberStatus"
  val etmpPathToMemberType:                                 JsPath = __ \ "memberType"
  val etmpPathToValueOfUnauthorisedPayment:                 JsPath = __ \ "valueOfUnauthorisedPayment"
  val etmpPathToWhoReceivedUnauthPayment:                   JsPath = __ \ "whoReceivedUnauthPayment"


  // UA
  val uaPathToAmendedVersion:                               JsPath = __ \ "amendedVersion"
  val uaPathToBenefitInKindBriefDescription:                JsPath = __ \ "benefitInKindBriefDescription"
  val uaPathToBenefitsPaidEarly:                            JsPath = __ \ "benefitsPaidEarly"
  val uaPathToCompanyAddress:                               JsPath = __ \ "employerAddress"

  private val uaPathToCompanyDetails:                       JsPath = __ \ "event1" \ "companyDetails"
  val uaPathToCompanyName:                                  JsPath = uaPathToCompanyDetails \ "companyName"
  val uaPathToCompanyNumber:                                JsPath = uaPathToCompanyDetails \ "companyNumber"

  val uaPathToDoYouHoldSignedMandate:                       JsPath = __ \ "doYouHoldSignedMandate"
  val uaPathToEmployerResidentialAddress:                   JsPath = __ \ "employerResidentialAddress"
  val uaPathToEmployerTangibleMoveableProperty:             JsPath = __ \ "employerTangibleMoveableProperty"
  val uaPathToErrorDescription:                             JsPath = __ \ "errorDescription"

  private val uaPathToIndividualMemberDetails:              JsPath = __ \ "membersDetails"
  val uaPathToIndividualMemberFirstName:                    JsPath = uaPathToIndividualMemberDetails \ "firstName"
  val uaPathToIndividualMemberLastName:                     JsPath = uaPathToIndividualMemberDetails \ "lastName"
  val uaPathToIndividualMemberNino:                         JsPath = uaPathToIndividualMemberDetails \ "nino"

  private val uaPathToLoanDetails:                          JsPath = __ \ "loanDetails"
  val uaPathToFundValue:                                    JsPath = uaPathToLoanDetails \ "fundValue"
  val uaPathToLoanAmount:                                   JsPath = uaPathToLoanDetails \ "loanAmount"

  val uaPathToMembersOrEmployers:                           JsPath = __ \ "event1" \ "membersOrEmployers"
  val uaPathToMemberPaymentNatureDescription:               JsPath = __ \ "memberPaymentNatureDescription"
  val uaPathToMemberResidentialAddress:                     JsPath = __ \ "memberResidentialAddress"
  val uaPathToMemberStatus:                                 JsPath = __ \ "memberStatus"
  val uaPathToMemberTangibleMoveableProperty:               JsPath = __ \ "memberTangibleMoveableProperty"
  val uaPathToPaymentNatureDesc:                            JsPath = __ \ "paymentNatureDesc"
  val uaPathToPaymentNatureEmployer:                        JsPath = __ \ "paymentNatureEmployer"
  val uaPathToPaymentNatureMember:                          JsPath = __ \ "paymentNatureMember"

  private val uaPathToPaymentValueAndDate:                  JsPath = __ \ "paymentValueAndDate"
  val uaPathToPaymentDate:                                  JsPath = uaPathToPaymentValueAndDate \ "paymentDate"
  val uaPathToPaymentValue:                                 JsPath = uaPathToPaymentValueAndDate \ "paymentValue"

  val uaPathToReasonForTheOverpaymentOrWriteOff:            JsPath = __ \ "reasonForTheOverpaymentOrWriteOff"
  val uaPathToRefundOfContributions:                        JsPath = __ \ "refundOfContributions"

  private val uaPathToSchemeDetails:                        JsPath = __ \ "schemeDetails"
  val uaPathToSchemeName:                                   JsPath = uaPathToSchemeDetails \ "schemeName"
  val uaPathToReference:                                    JsPath = uaPathToSchemeDetails \ "reference"

  val uaPathToSchemeUnAuthPaySurchargeMember:               JsPath = __ \ "schemeUnAuthPaySurchargeMember"
  val uaPathToUnauthorisedPaymentRecipientName:             JsPath = __ \ "unauthorisedPaymentRecipientName"
  val uaPathToValueOfUnauthorisedPayment:                   JsPath = __ \ "valueOfUnauthorisedPayment"
  val uaPathToWhoWasTheTransferMade:                        JsPath = __ \ "whoWasTheTransferMade"
}
