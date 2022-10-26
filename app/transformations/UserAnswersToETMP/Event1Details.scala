/*
 * Copyright 2022 HM Revenue & Customs
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

object Event1Details extends Transformer {
  private val paymentNatureTypeKeyBenefitInKind: String = "benefitInKind"
  private val paymentNatureTypeKeyTransferToNonRegPensionScheme: String = "transferToNonRegPensionScheme"
  private val paymentNatureTypeKeyRefundOfContributions: String = "refundOfContributions"
  private val paymentNatureTypeKeyOverpaymentOrWriteOff: String = "overpaymentOrWriteOff"
  private val paymentNatureTypeKeyResidentialPropertyHeld: String = "residentialPropertyHeld"
  private val paymentNatureTypeKeyTangibleMoveablePropertyHeld: String = "tangibleMoveablePropertyHeld"
  private val paymentNatureTypeKeyErrorCalcTaxFreeLumpSums: String = "errorCalcTaxFreeLumpSums"
  private val paymentNatureTypeKeyCourtOrConfiscationOrder: String = "courtOrConfiscationOrder"
  private val paymentNatureTypeKeyLoansExceeding50PercentOfFundValue: String = "loansExceeding50PercentOfFundValue"
  private val paymentNatureTypeKeyOther: String = "other"
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
    paymentNatureTypeKeyResidentialPropertyHeld -> "Residential property held directly or indirectly by an investment-regulated pension scheme",
    paymentNatureTypeKeyTangibleMoveablePropertyHeld -> "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme",
    paymentNatureTypeKeyCourtOrConfiscationOrder -> "Court Order Payment/Confiscation Order",
    paymentNatureTypeKeyOther -> "Other"
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

  private def freeTxtOrSchemeOrRecipientName(paymentNature: String, whoReceivedUnauthorisedPayment: String): Reads[JsString] = {
    (paymentNature, whoReceivedUnauthorisedPayment) match {
      case (`paymentNatureTypeKeyBenefitInKind`, `whoReceivedUnauthPaymentIndividual`) => (__ \ 'benefitInKindBriefDescription).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyTransferToNonRegPensionScheme`, `whoReceivedUnauthPaymentIndividual`) => (__ \ 'schemeDetails \ 'schemeName).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyErrorCalcTaxFreeLumpSums`, `whoReceivedUnauthPaymentIndividual`) => (__ \ 'errorDescription).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyBenefitsPaidEarly`, `whoReceivedUnauthPaymentIndividual`) => (__ \ 'benefitsPaidEarly).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyTangibleMoveablePropertyHeld`, `whoReceivedUnauthPaymentIndividual`) => (__ \ 'memberTangibleMoveableProperty).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyTangibleMoveablePropertyHeld`, `whoReceivedUnauthPaymentEmployer`) => (__ \ 'employerTangibleMoveableProperty).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyCourtOrConfiscationOrder`, _) => (__ \ 'unauthorisedPaymentRecipientName).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyOther`, `whoReceivedUnauthPaymentIndividual`) => (__ \ 'memberPaymentNatureDescription).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyOther`, `whoReceivedUnauthPaymentEmployer`) => (__ \ 'paymentNatureDesc).json.pick.map(_.as[JsString])
      case _ => Reads[JsString](_ => JsError(""))
    }
  }

  private def pstrOrReference(paymentNature: String): Reads[JsString] = {
    paymentNature match {
      case `paymentNatureTypeKeyTransferToNonRegPensionScheme` => (__ \ 'schemeDetails \ 'reference).json.pick.map(_.as[JsString])
      case _ => Reads[JsString](_ => JsError(""))
    }
  }

  private val pathIndividualMemberDetails = __ \ 'individualMemberDetails
  private val pathEmployerMemberDetails = __ \ 'employerMemDetails
  private val pathUnauthorisedPaymentDetails = __ \ 'unAuthorisedPaymentDetails
  private val readsPaymentNature: Reads[String] = (__ \ 'paymentNature).json.pick.map(_.as[JsString].value)

  private val readsWhoReceivedUnauthorisedPayment: Reads[String] = {
    (__ \ 'whoReceivedUnauthPayment).json.pick.flatMap {
      case JsString("member") => Reads.pure[String](whoReceivedUnauthPaymentIndividual)
      case JsString("employer") => Reads.pure[String](whoReceivedUnauthPaymentEmployer)
      case _ => fail
    }
  }

  val schemePayingSurcharge: Reads[JsObject] =
    (__ \ 'valueOfUnauthorisedPayment).read[Boolean].flatMap {
      case true => (pathIndividualMemberDetails \ 'schemePayingSurcharge).json
        .copyFrom((__ \ 'schemeUnAuthPaySurchargeMember).json.pick.map(toYesNo))
      case _ => fail
    }

  private val readsIndividualMemberDetails: Reads[JsObject] =
    ((pathIndividualMemberDetails \ 'firstName).json.copyFrom((__ \ 'membersDetails \ 'firstName).json.pick) and
      (pathIndividualMemberDetails \ 'lastName).json.copyFrom((__ \ 'membersDetails \ 'lastName).json.pick) and
      (pathIndividualMemberDetails \ 'nino).json.copyFrom((__ \ 'membersDetails \ 'nino).json.pick) and
      (pathIndividualMemberDetails \ 'signedMandate).json.copyFrom((__ \ 'doYouHoldSignedMandate).json.pick.map(toYesNo)) and
      (pathIndividualMemberDetails \ 'pmtMoreThan25PerFundValue).json.copyFrom((__ \ 'valueOfUnauthorisedPayment).json.pick.map(toYesNo)) and
      schemePayingSurcharge.orElse(doNothing)).reduce

  private val readsEmployerDetails: Reads[JsObject] =
    ((pathEmployerMemberDetails \ 'compOrOrgName).json.copyFrom((__ \ 'event1 \ 'companyDetails \ 'companyName).json.pick) and
      (pathEmployerMemberDetails \ 'crnNumber).json.copyFrom((__ \ 'event1 \ 'companyDetails \ 'companyNumber).json.pick) and
      (pathEmployerMemberDetails \ 'addressDetails).json.copyFrom(readsAddress(__ \ 'event1 \ 'employerAddress)).orElse(doNothing)
      ).reduce

  private def readsUnauthorisedPaymentDetails(paymentNature: String, whoReceivedUnauthorisedPayment: String): Reads[JsObject] = {
    def readsPaymentType2: Reads[JsString] = paymentNature match {
      case `paymentNatureTypeKeyTransferToNonRegPensionScheme` =>
        (__ \ 'whoWasTheTransferMade).json.pick.map(jsValue => JsString(whoWasTransferMadeToMap(jsValue.as[JsString].value)))
      case `paymentNatureTypeKeyRefundOfContributions` =>
        (__ \ 'refundOfContributions).json.pick.map(jsValue => JsString(refundOfContributionsMap(jsValue.as[JsString].value)))
      case `paymentNatureTypeKeyOverpaymentOrWriteOff` =>
        (__ \ 'reasonForTheOverpaymentOrWriteOff).json.pick.map(jsValue => JsString(overpaymentOrWriteOffMap(jsValue.as[JsString].value)))
      case _ => fail
    }

    val readsResidentialAddressMember: Reads[JsObject] = paymentNature match {
      case `paymentNatureTypeKeyResidentialPropertyHeld` => readsAddress(__ \ 'event1 \ 'memberResidentialAddress)
      case _ => fail
    }

    val readsResidentialAddressEmployer: Reads[JsObject] = paymentNature match {
      case `paymentNatureTypeKeyResidentialPropertyHeld` => readsAddress(__ \ 'event1 \ 'employerResidentialAddress)
      case _ => fail
    }

    val loanPymtPgs: Reads[JsObject] = paymentNature match {
      case `paymentNatureTypeKeyLoansExceeding50PercentOfFundValue` =>
        ((pathUnauthorisedPaymentDetails \ 'pmtAmtOrLoanAmt).json.copyFrom((__ \ 'loanDetails \ 'loanAmount).json.pick) and
          (pathUnauthorisedPaymentDetails \ 'fundValue).json.copyFrom((__ \ 'loanDetails \ 'fundValue).json.pick)).reduce
      case `paymentNatureTypeKeyCourtOrConfiscationOrder` =>
        (pathUnauthorisedPaymentDetails \ 'pmtAmtOrLoanAmt).json.copyFrom((__ \ 'loanDetails \ 'loanAmount).json.pick)
      case _ => fail
    }

    whoReceivedUnauthorisedPayment match {
      case `whoReceivedUnauthPaymentIndividual` =>
        ((pathUnauthorisedPaymentDetails \ 'unAuthorisedPmtType1).json.put(JsString(paymentNatureMemberMap(paymentNature))) and
          (pathUnauthorisedPaymentDetails \ 'freeTxtOrSchemeOrRecipientName).json.copyFrom(freeTxtOrSchemeOrRecipientName(paymentNature, whoReceivedUnauthorisedPayment)).orElse(doNothing) and
          (pathUnauthorisedPaymentDetails \ 'pstrOrReference).json.copyFrom(pstrOrReference(paymentNature)).orElse(doNothing) and
          (pathUnauthorisedPaymentDetails \ 'unAuthorisedPmtType2).json.copyFrom(readsPaymentType2).orElse(doNothing) and
          (pathUnauthorisedPaymentDetails \ 'valueOfUnauthorisedPayment).json.copyFrom((__ \ 'paymentValueAndDate \ 'paymentValue).json.pick) and
          (pathUnauthorisedPaymentDetails \ 'dateOfUnauthorisedPayment).json.copyFrom((__ \ 'paymentValueAndDate \ 'paymentDate).json.pick) and
          (pathUnauthorisedPaymentDetails \ 'residentialPropertyAddress).json.copyFrom(readsResidentialAddressMember).orElse(doNothing)
          ).reduce
      case `whoReceivedUnauthPaymentEmployer` =>
        ((pathUnauthorisedPaymentDetails \ 'unAuthorisedPmtType1).json.put(JsString(paymentNatureEmployerMap(paymentNature))) and
          (pathUnauthorisedPaymentDetails \ 'freeTxtOrSchemeOrRecipientName).json.copyFrom(freeTxtOrSchemeOrRecipientName(paymentNature, whoReceivedUnauthorisedPayment)).orElse(doNothing) and
          loanPymtPgs.orElse(doNothing) and
          (pathUnauthorisedPaymentDetails \ 'residentialPropertyAddress).json.copyFrom(readsResidentialAddressEmployer).orElse(doNothing) and
          (pathUnauthorisedPaymentDetails \ 'valueOfUnauthorisedPayment).json.copyFrom((__ \ 'paymentValueAndDate \ 'paymentValue).json.pick) and
          (pathUnauthorisedPaymentDetails \ 'dateOfUnauthorisedPayment).json.copyFrom((__ \ 'paymentValueAndDate \ 'paymentDate).json.pick)
          ).reduce
      case _ => fail
    }
  }

  private def readsMemberOrEmployer(whoReceivedUnauthorisedPayment: String): Reads[JsObject] = {
    whoReceivedUnauthorisedPayment match {
      case `whoReceivedUnauthPaymentIndividual` => readsIndividualMemberDetails
      case `whoReceivedUnauthPaymentEmployer` => readsEmployerDetails
    }
  }

  private val readsMember: Reads[JsObject] = {
    (for {
      paymentNature <- readsPaymentNature
      whoReceivedUnauthorisedPayment <- readsWhoReceivedUnauthorisedPayment
    } yield {
      (
        (__ \ 'memberType).json.put(JsString(whoReceivedUnauthorisedPayment)) and
          (__ \ 'memberStatus).json.put(JsString("New")) and
          readsMemberOrEmployer(whoReceivedUnauthorisedPayment) and
          readsUnauthorisedPaymentDetails(paymentNature, whoReceivedUnauthorisedPayment)
        ).reduce
    }).flatMap[JsObject](identity)
  }

  val transformToETMPData: Reads[JsObject] = {
    (__ \ 'membersOrEmployers).readNullable[JsArray](__.read(Reads.seq(readsMember))
      .map(JsArray(_))).map { optionJsArray =>
      val jsonArray = optionJsArray.getOrElse(Json.arr())
      Json.obj(
        "eventReportDetails" -> Json.obj(
          "reportStartDate" -> "2020-09-01",
          "reportEndDate" -> "2020-09-01"
        ),
        "event1Details" -> Json.obj(
          "event1Details" -> jsonArray
        )
      )
    }
  }
}

