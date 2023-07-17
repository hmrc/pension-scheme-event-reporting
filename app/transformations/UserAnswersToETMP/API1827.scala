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

object API1827 extends Transformer {
  private val pathIndividualMemberDetails = __ \ Symbol("individualMemberDetails")
  private val pathEmployerMemberDetails = __ \ Symbol("employerMemDetails")
  private val pathUnauthorisedPaymentDetails = __ \ Symbol("unAuthorisedPaymentDetails")

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
        (__ \ Symbol("benefitInKindBriefDescription")).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyTransferToNonRegPensionScheme`, `whoReceivedUnauthPaymentIndividual`) =>
        (__ \ Symbol("schemeDetails") \ Symbol("schemeName")).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyErrorCalcTaxFreeLumpSums`, `whoReceivedUnauthPaymentIndividual`) =>
        (__ \ Symbol("errorDescription")).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyBenefitsPaidEarly`, `whoReceivedUnauthPaymentIndividual`) =>
        (__ \ Symbol("benefitsPaidEarly")).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyTangibleMoveablePropertyHeld`, `whoReceivedUnauthPaymentIndividual`) =>
        (__ \ Symbol("memberTangibleMoveableProperty")).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyTangibleMoveablePropertyHeldEmployer`, `whoReceivedUnauthPaymentEmployer`) =>
        (__ \ Symbol("employerTangibleMoveableProperty")).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyCourtOrConfiscationOrder`, _) =>
        (__ \ Symbol("unauthorisedPaymentRecipientName")).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyCourtOrConfiscationOrderEmployer`, _) =>
        (__ \ Symbol("unauthorisedPaymentRecipientName")).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyOther`, `whoReceivedUnauthPaymentIndividual`) =>
        (__ \ Symbol("memberPaymentNatureDescription")).json.pick.map(_.as[JsString])
      case (`paymentNatureTypeKeyOtherEmployer`, `whoReceivedUnauthPaymentEmployer`) =>
        (__ \ Symbol("paymentNatureDesc")).json.pick.map(_.as[JsString])
      case _ => Reads[JsString](_ => JsError(""))
    }
  }

  private def pstrOrReference(paymentNature: String): Reads[JsString] = {
    paymentNature match {
      case `paymentNatureTypeKeyTransferToNonRegPensionScheme` => (__ \ Symbol("schemeDetails") \ Symbol("reference")).json.pick.map(_.as[JsString])
      case _ => Reads[JsString](_ => JsError(""))
    }
  }

  private val readsPaymentNatureMember: Reads[String] = (__ \ Symbol("paymentNatureMember")).json.pick.map(_.as[JsString].value)
  private val readsPaymentNatureEmployer: Reads[String] = (__ \ Symbol("paymentNatureEmployer")).json.pick.map(_.as[JsString].value)

  private val readsMemberStatus = {
    (__ \ Symbol("memberStatus")).json.pick
  }

  private val readsAmendedVersion = {
    (__ \ Symbol("amendedVersion")).json.pick
  }

  private val readsWhoReceivedUnauthorisedPayment: Reads[String] = {
    (__ \ Symbol("whoReceivedUnauthPayment")).json.pick.flatMap {
      case JsString("member") => Reads.pure[String](whoReceivedUnauthPaymentIndividual)
      case JsString("employer") => Reads.pure[String](whoReceivedUnauthPaymentEmployer)
      case _ => fail
    }
  }

  private val schemePayingSurcharge: Reads[JsObject] =
    (__ \ Symbol("valueOfUnauthorisedPayment")).read[Boolean].flatMap {
      case true => (pathIndividualMemberDetails \ Symbol("schemePayingSurcharge")).json
        .copyFrom((__ \ Symbol("schemeUnAuthPaySurchargeMember")).json.pick.map(toYesNo))
      case _ => fail
    }

  private val readsIndividualMemberDetails: Reads[JsObject] =
    ((pathIndividualMemberDetails \ Symbol("firstName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("firstName")).json.pick) and
      (pathIndividualMemberDetails \ Symbol("lastName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("lastName")).json.pick) and
      (pathIndividualMemberDetails \ Symbol("nino")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("nino")).json.pick) and
      (pathIndividualMemberDetails \ Symbol("signedMandate")).json.copyFrom((__ \ Symbol("doYouHoldSignedMandate")).json.pick.map(toYesNo)) and
      (pathIndividualMemberDetails \ Symbol("pmtMoreThan25PerFundValue")).json.copyFrom((__ \ Symbol("valueOfUnauthorisedPayment")).json.pick.map(toYesNo)) and
      schemePayingSurcharge.orElse(doNothing)).reduce

  private val readsEmployerDetails: Reads[JsObject] =
    ((pathEmployerMemberDetails \ Symbol("compOrOrgName")).json
      .copyFrom((__ \ Symbol("event1") \ Symbol("companyDetails") \ Symbol("companyName")).json.pick) and
      (pathEmployerMemberDetails \ Symbol("crnNumber")).json
        .copyFrom((__ \ Symbol("event1") \ Symbol("companyDetails") \ Symbol("companyNumber")).json.pick) and
      (pathEmployerMemberDetails \ Symbol("addressDetails")).json.copyFrom(readsAddress(__ \ Symbol("employerAddress"))).orElse(doNothing)
      ).reduce

  private def loanPymtPgs(paymentNature: String): Reads[JsObject] = {
    def readsLoanAmount: Reads[JsObject] =
      (pathUnauthorisedPaymentDetails \ Symbol("pmtAmtOrLoanAmt")).json.copyFrom((__ \ Symbol("loanDetails") \ Symbol("loanAmount")).json.pick)

    def readsFundValue: Reads[JsObject] = (pathUnauthorisedPaymentDetails \ Symbol("fundValue")).json
      .copyFrom((__ \ Symbol("loanDetails") \ Symbol("fundValue")).json.pick)

    paymentNature match {
      case `paymentNatureTypeKeyLoansExceeding50PercentOfFundValue` => (readsLoanAmount and readsFundValue).reduce
      case `paymentNatureTypeKeyCourtOrConfiscationOrderEmployer` => readsLoanAmount
      case _ => fail
    }
  }

  private def readsPaymentType2(paymentNature: String): Reads[JsString] = paymentNature match {
    case `paymentNatureTypeKeyTransferToNonRegPensionScheme` =>
      (__ \ Symbol("whoWasTheTransferMade")).json.pick.map(jsValue => JsString(whoWasTransferMadeToMap(jsValue.as[JsString].value)))
    case `paymentNatureTypeKeyRefundOfContributions` =>
      (__ \ Symbol("refundOfContributions")).json.pick.map(jsValue => JsString(refundOfContributionsMap(jsValue.as[JsString].value)))
    case `paymentNatureTypeKeyOverpaymentOrWriteOff` =>
      (__ \ Symbol("reasonForTheOverpaymentOrWriteOff")).json.pick.map(jsValue => JsString(overpaymentOrWriteOffMap(jsValue.as[JsString].value)))
    case _ => fail
  }

  private def readsUnauthorisedPaymentDetails(paymentNature: String, whoReceivedUnauthorisedPayment: String): Reads[JsObject] = {
    val readsResidentialAddressMember: Reads[JsObject] = paymentNature match {
      case `paymentNatureTypeKeyResidentialPropertyHeld` => readsAddress(__ \ Symbol("memberResidentialAddress"))
      case _ => fail
    }

    val readsResidentialAddressEmployer: Reads[JsObject] = paymentNature match {
      case `paymentNatureTypeKeyResidentialPropertyHeldEmployer` => readsAddress(__ \ Symbol("employerResidentialAddress"))
      case _ => fail
    }

    whoReceivedUnauthorisedPayment match {
      case `whoReceivedUnauthPaymentIndividual` =>
        ((pathUnauthorisedPaymentDetails \ Symbol("unAuthorisedPmtType1")).json.put(JsString(paymentNatureMemberMap(paymentNature))) and
          (pathUnauthorisedPaymentDetails \ Symbol("freeTxtOrSchemeOrRecipientName")).json
            .copyFrom(freeTxtOrSchemeOrRecipientName(paymentNature, whoReceivedUnauthorisedPayment)).orElse(doNothing) and
          (pathUnauthorisedPaymentDetails \ Symbol("pstrOrReference")).json.copyFrom(pstrOrReference(paymentNature)).orElse(doNothing) and
          (pathUnauthorisedPaymentDetails \ Symbol("unAuthorisedPmtType2")).json.copyFrom(readsPaymentType2(paymentNature)).orElse(doNothing) and
          (pathUnauthorisedPaymentDetails \ Symbol("valueOfUnauthorisedPayment")).json
            .copyFrom((__ \ Symbol("paymentValueAndDate") \ Symbol("paymentValue")).json.pick) and
          (pathUnauthorisedPaymentDetails \ Symbol("dateOfUnauthorisedPayment")).json
            .copyFrom((__ \ Symbol("paymentValueAndDate") \ Symbol("paymentDate")).json.pick) and
          (pathUnauthorisedPaymentDetails \ Symbol("residentialPropertyAddress")).json
            .copyFrom(readsResidentialAddressMember).orElse(doNothing)
          ).reduce
      case `whoReceivedUnauthPaymentEmployer` =>
        ((pathUnauthorisedPaymentDetails \ Symbol("unAuthorisedPmtType1")).json.put(JsString(paymentNatureEmployerMap(paymentNature))) and
          (pathUnauthorisedPaymentDetails \ Symbol("freeTxtOrSchemeOrRecipientName")).json
            .copyFrom(freeTxtOrSchemeOrRecipientName(paymentNature, whoReceivedUnauthorisedPayment)).orElse(doNothing) and
          loanPymtPgs(paymentNature).orElse(doNothing) and
          (pathUnauthorisedPaymentDetails \ Symbol("residentialPropertyAddress")).json
            .copyFrom(readsResidentialAddressEmployer).orElse(doNothing) and
          (pathUnauthorisedPaymentDetails \ Symbol("valueOfUnauthorisedPayment")).json
            .copyFrom((__ \ Symbol("paymentValueAndDate") \ Symbol("paymentValue")).json.pick) and
          (pathUnauthorisedPaymentDetails \ Symbol("dateOfUnauthorisedPayment")).json
            .copyFrom((__ \ Symbol("paymentValueAndDate") \ Symbol("paymentDate")).json.pick)
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

  private val readsMember: Reads[JsObject] = {
    (for {
      whoReceivedUnauthorisedPayment <- readsWhoReceivedUnauthorisedPayment
      paymentNature <- whoReceivedUnauthorisedPayment match {
        case `whoReceivedUnauthPaymentIndividual` => readsPaymentNatureMember
        case `whoReceivedUnauthPaymentEmployer` => readsPaymentNatureEmployer
        case _ => fail
      }
    } yield {
      (
        (__ \ Symbol("memberType")).json.put(JsString(whoReceivedUnauthorisedPayment)) and
          (__ \ Symbol("memberStatus")).json.copyFrom(readsMemberStatus) and
          (__ \ Symbol("amendedVersion")).json.copyFrom(readsAmendedVersion) and
          readsMemberOrEmployer(whoReceivedUnauthorisedPayment) and
          readsUnauthorisedPaymentDetails(paymentNature, whoReceivedUnauthorisedPayment)
        ).reduce
    }).flatMap[JsObject](identity)
  }

  val transformToETMPData: Reads[JsObject] = {
    val reads = (__ \ Symbol("event1") \ Symbol("membersOrEmployers")).readNullable[JsArray](__.read(Reads.seq(readsMember))
      .map(JsArray(_))).map { optionJsArray =>
      val jsonArray = optionJsArray.getOrElse(Json.arr())
      Json.obj(
        "event1Details" -> Json.obj(
          "event1Details" -> jsonArray
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

