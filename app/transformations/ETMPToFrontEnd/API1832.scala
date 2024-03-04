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

package transformations.ETMPToFrontEnd

import models.enumeration.EventType
import models.enumeration.EventType.{Event2, Event24, Event3, Event4, Event5, Event6, Event7, Event8, Event8A}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer


object API1832 {

  import transformations.ETMPToFrontEnd.API1832ReadsUtilities._
  import transformations.ETMPToFrontEnd.MemberEventReportPaths._

  def rds1832Api(eventType: EventType): Reads[JsObject] = {
    pathEtmpEventDetails.readNullable(readsMembers(eventType)).flatMap {
      case None => Reads.pure(Json.obj())
      case Some(y) =>
        pathUaEventDetailsForEventType(eventType).json.put(y)
    }
  }

  private def readsMembers(eventType: EventType): Reads[JsArray] = __.read(Reads.seq(readsMemberDetailsByEventType(eventType))).map(JsArray(_))

  private def readsMemberDetailsByEventType(eventType: EventType): Reads[JsObject] = eventType match {
    case Event2 => rdsMemberDetailsEvent2
    case Event3 => rdsMemberDetailsEvent3
    case Event4 => rdsMemberDetailsEvent4
    case Event5 => rdsMemberDetailsEvent5
    case Event6 => rdsMemberDetailsEvent6
    case Event7 => rdsMemberDetailsEvent7
    case Event8 => rdsMemberDetailsEvent8
    case Event8A => rdsMemberDetailsEvent8A
    case Event24 => rdsMemberDetailsEvent24
    case _ => rdsMemberDetailsEvent22And23
  }

  private val rdsMemberDetailsEvent2: Reads[JsObject] = (readsDeceasedMemberDetails and readsBeneficiaryDetails and readsEvent2PaymentDetails).reduce
  private val rdsMemberDetailsEvent3: Reads[JsObject] = (readsMemberDetails and readsEvent3PaymentDetails).reduce
  private val rdsMemberDetailsEvent4: Reads[JsObject] = (readsMemberDetails and readsEvent4PaymentDetails).reduce
  private val rdsMemberDetailsEvent5: Reads[JsObject] = (readsMemberDetails and readsEvent5PaymentDetails).reduce
  private val rdsMemberDetailsEvent6: Reads[JsObject] = (readsMemberDetails and readsEvent6PaymentDetails).reduce
  private val rdsMemberDetailsEvent7: Reads[JsObject] = (readsMemberDetails and readsEvent7PaymentDetails).reduce
  private val rdsMemberDetailsEvent8: Reads[JsObject] = (readsMemberDetails and readsEvent8PaymentDetails).reduce
  private val rdsMemberDetailsEvent8A: Reads[JsObject] = (readsMemberDetails and readsEvent8APaymentDetails).reduce
  private val rdsMemberDetailsEvent22And23: Reads[JsObject] = (readsMemberDetails and readsEvent22Or23PaymentDetails).reduce
  private val rdsMemberDetailsEvent24: Reads[JsObject] = (readsMemberDetails and readsEvent24PaymentDetails).reduce
}

private object API1832ReadsUtilities extends Transformer {

  import transformations.ETMPToFrontEnd.MemberEventReportPaths._

  lazy val readsEvent2PaymentDetails: Reads[JsObject] = {
    (
      pathUaAmountPaid.json.copyFrom(pathEtmpAmountPaid.json.pick) and
        pathUaDatePaid.json.copyFrom(pathEtmpEventDate.json.pick)
      ).reduce
  }

  lazy val readsEvent3PaymentDetails: Reads[JsObject] = {
    (
      pathUaReasonBenefitTaken.json.copyFrom(readsReasonBenefitTakenEvent3) and
        pathUaFreeText.json.copyFrom(pathEtmpFreeText.json.pick).orElse(doNothing) and
        pathUaEventDate.json.copyFrom(pathEtmpEventDate.json.pick) and
        pathUaAmountPaidNested.json.copyFrom(pathEtmpAmountBenefit.json.pick)
      ).reduce
  }

  lazy val readsEvent4PaymentDetails: Reads[JsObject] = {
    (
      pathUaAmountPaidNested.json.copyFrom(pathEtmpAmountPaid.json.pick) and
        pathUaEventDate.json.copyFrom(pathEtmpEventDate.json.pick)
      ).reduce
  }

  lazy val readsEvent5PaymentDetails: Reads[JsObject] = {
    (
      pathUaAmountPaidNested.json.copyFrom(pathEtmpAnnualRate.json.pick) and
        pathUaEventDate.json.copyFrom(pathEtmpEventDate.json.pick)
      ).reduce
  }

  lazy val readsEvent6PaymentDetails: Reads[JsObject] = {
    (
      pathUaTypeOfProtection.json.copyFrom(readsTypeOfProtectionEvent6) and
        pathUaInputProtectionType.json.copyFrom(pathEtmpFreeText.json.pick) and
        pathUaAmountCrystallised.json.copyFrom(pathEtmpAmountCrystalised.json.pick) and
        pathUaCrystallisedDate.json.copyFrom(pathEtmpEventDate.json.pick)
      ).reduce
  }

  lazy val readsEvent7PaymentDetails: Reads[JsObject] = {
    (
      pathUaLumpSumAmount.json.copyFrom(pathEtmpAmountLumpSum.json.pick) and
        pathUaCrystallisedAmount.json.copyFrom(pathEtmpAmountCrystalised.json.pick) and
        pathUaPaymentDate.json.copyFrom(pathEtmpEventDate.json.pick)
      ).reduce
  }

  lazy val readsEvent8PaymentDetails: Reads[JsObject] = {
    (
      pathUaTypeOfProtection.json.copyFrom(readsTypeOfProtectionEvent8) and
        pathUaTypeOfProtectionReference.json.copyFrom(pathEtmpFreeText.json.pick) and
        pathUaLumpSumAmountNested.json.copyFrom(pathEtmpAmountLumpSum.json.pick) and
        pathUaLumpSumDateNested.json.copyFrom(pathEtmpEventDate.json.pick)
      ).reduce
  }

  def yesNoTransform(js: JsValue, err: String):Reads[JsBoolean] = {
    js match {
      case JsString(value) => value match {
        case "Yes" => Reads.pure(JsBoolean(true))
        case "No" => Reads.pure(JsBoolean(false))
        case _ => Reads.failed(err)
      }
      case _ => Reads.failed(err)
    }
  }

  lazy val readsEvent24PaymentDetails: Reads[JsObject] = {
    (
      pathUaMemberHoldProtection.json.copyFrom(pathEtmpMemberHoldProtectionEvent24.json.pick.flatMap(yesNoTransform(_, "hold protection failed"))) and
        pathUaTypeOfProtectionEvent24.json.copyFrom(readsTypeOfProtectionEvent24).orElse(doNothing) and
        pathUaProtectionGroup1Event24.json.copyFrom(readsTypeOfProtectionGroup1Event24).orElse(doNothing) and
        pathUaPreCommenceReference.json.copyFrom(pathEtmpPreCommenceReference.json.pick).orElse(doNothing) and
        pathUaPensionCreditReference.json.copyFrom(pathEtmpPensionCreditReference.json.pick).orElse(doNothing) and
        pathUaNonResidenceReference.json.copyFrom(pathEtmpNonResidenceReference.json.pick).orElse(doNothing) and
        pathUaOverseasReference.json.copyFrom(pathEtmpOverseasReference.json.pick).orElse(doNothing) and
        pathUaAvailableLumpSumExceeded.json.copyFrom(pathEtmpAvailableLumpSumExceeded.json.pick.flatMap(yesNoTransform(_, "available lump sum exceeded failed"))) and
        pathUaAvailableLumpSumDBAExceeded.json.copyFrom(pathEtmpAvailableLumpSumDBAExceeded.json.pick.flatMap(yesNoTransform(_, "available lump sum DBA exceeded failed"))).orElse(doNothing) and
        pathUaAmountCrystalised.json.copyFrom(pathEtmpAmountCrystalised.json.pick) and
        pathUaBCEType.json.copyFrom(readsBCETypeEvent24) and
        pathUaCrystallisedDateEvent24.json.copyFrom(pathEtmpTaxYearEndingDate.json.pick) and
        pathUaFreeTextEvent24.json.copyFrom(pathEtmpFreeText.json.pick).orElse(doNothing) and
        pathUaMarginalRate.json.copyFrom(pathEtmpMarginalRate.json.pick.flatMap(yesNoTransform(_, "taxed at marginal rate failed"))).orElse(doNothing) and
        pathUaPayeReference.json.copyFrom(pathEtmpPayeReference.json.pick).orElse(doNothing)
    ).reduce
  }

  private def readAdditiveIfPresent(etmpPath: JsPath, uaPath: JsPath, f: String => String = identity): JsObject => Reads[JsObject] = jsObj =>
    etmpPath.readNullable[String].flatMap {
      case None => Reads.pure(jsObj)
      case Some(str) => uaPath.json.put(JsString(f(str))).map(_ ++ jsObj)
    }

  lazy val readsEvent8APaymentDetails: Reads[JsObject] = {
    (
      pathUaLumpSumAmountNested.json.copyFrom(pathEtmpAmountLumpSum.json.pick) and
        pathUaLumpSumDateNested.json.copyFrom(pathEtmpEventDate.json.pick)
      ).reduce
      .flatMap(readAdditiveIfPresent(pathEtmpTypeOfProtection, pathUaTypeOfProtection, typeOfProtectionUAEvent8A))
      .flatMap(readAdditiveIfPresent(pathEtmpReasonBenefitTaken, pathUaPaymentType, paymentTypeUAEvent8A))
      .flatMap(readAdditiveIfPresent(pathEtmpFreeText, pathUaTypeOfProtectionReference))
  }

  lazy val readsEvent22Or23PaymentDetails: Reads[JsObject] = {
    (
      pathUaChooseTaxYearEvent.json.copyFrom(readsTaxYearEndDateEvent22And23) and
        pathUaTotalPensionAmounts.json.copyFrom(pathEtmpMonetaryAmount.json.pick)
      ).reduce
  }

  lazy val readsMemberDetails: Reads[JsObject] = {
    (
      (pathUaMembersDetails \ Symbol("firstName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("firstName")).json.pick) and
        (pathUaMembersDetails \ Symbol("lastName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("lastName")).json.pick) and
        (pathUaMembersDetails \ Symbol("nino")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("nino")).json.pick) and
        pathUaMemberStatus.json.copyFrom(pathEtmlMemberStatus.json.pick).orElse(doNothing) and
        pathUaAmendedVersion.json.copyFrom(pathEtmlAmendedVersion.json.pick).orElse(doNothing)
      ).reduce
  }

  lazy val readsDeceasedMemberDetails: Reads[JsObject] = {
    (
      (pathUaDeceasedMembersDetails \ Symbol("firstName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("firstName")).json.pick) and
        (pathUaDeceasedMembersDetails \ Symbol("lastName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("lastName")).json.pick) and
        (pathUaDeceasedMembersDetails \ Symbol("nino")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("nino")).json.pick) and
        pathUaMemberStatus.json.copyFrom(pathEtmlMemberStatus.json.pick).orElse(doNothing) and
        pathUaAmendedVersion.json.copyFrom(pathEtmlAmendedVersion.json.pick).orElse(doNothing)
      ).reduce
  }

  lazy val readsBeneficiaryDetails: Reads[JsObject] = {
    (
      (pathUaBeneficiaryDetails \ Symbol("firstName")).json.copyFrom((pathEtmpPersonReceivedThePayment \ Symbol("firstName")).json.pick) and
        (pathUaBeneficiaryDetails \ Symbol("lastName")).json.copyFrom((pathEtmpPersonReceivedThePayment \ Symbol("lastName")).json.pick) and
        (pathUaBeneficiaryDetails \ Symbol("nino")).json.copyFrom((pathEtmpPersonReceivedThePayment \ Symbol("nino")).json.pick)
      ).reduce
  }

  private lazy val readsTypeOfProtectionEvent6: Reads[JsString] = {
    pathEtmpTypeOfProtection.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(typeOfProtectionUAEvent6(str)))
      case _ => fail(JsString("typeOfProtectionEvent6"))
    }
  }

  private lazy val readsTypeOfProtectionEvent8: Reads[JsString] = {
    pathEtmpTypeOfProtection.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(typeOfProtectionUAEvent8(str)))
      case _ => fail(JsString("typeOfProtectionEvent8"))
    }
  }

  private lazy val readsReasonBenefitTakenEvent3: Reads[JsString] = {
    pathEtmpReasonBenefitTaken.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(reasonBenefitTakenUAEvent3(str)))
      case _ => fail(JsString("reasonBenefitTakenEvent3"))
    }
  }

  private lazy val readsTaxYearEndDateEvent22And23: Reads[JsString] = {
    pathEtmpTaxYearEndingDate.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString((str.substring(0, 4).toInt - 1).toString))
      case _ => fail(JsString("taxYearEndDateEvent22And23"))
    }
  }

  private lazy val readsTypeOfProtectionEvent24: Reads[JsString] = {
    pathEtmpTypeOfProtection.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(typeOfProtectionUAEvent24(str)))
      case _ => fail(JsString("typeOfProtectionEvent24"))
    }
  }

  private lazy val readsTypeOfProtectionGroup1Event24: Reads[JsArray] = {
    pathEtmpSchemeSpecificLumpSum.json.pick.flatMap {
      case JsString(str) if str == "Yes" =>
        Reads.pure(JsArray(Seq(JsString("schemeSpecific"))))
      case x =>
        fail(JsArray(Seq(JsString("typeOfProtectionGroup1Event24"))))
    }
  }

  private lazy val readsBCETypeEvent24: Reads[JsString] = {
    pathEtmpBCEType.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(bceTypeUAEvent24(str)))
      case _ => fail(JsString("bceTypeEvent24"))
    }
  }

  private def typeOfProtectionUAEvent6(tOP: String): String = tOP match {
    case "Enhanced life time allowance" => "enhancedLifetimeAllowance"
    case "Enhanced protection" => "enhancedProtection"
    case "Fixed protection" => "fixedProtection"
    case "Fixed protection 2014" => "fixedProtection2014"
    case "Fixed protection 2016" => "fixedProtection2016"
    case "Individual protection 2014" => "individualProtection2014"
    case "Individual protection 2016" => "individualProtection2016"
  }

  private def paymentTypeUAEvent8A(rBT: String): String = rBT match {
    case "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection"
    => "paymentOfAStandAloneLumpSum"
    case "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance"
    => "paymentOfASchemeSpecificLumpSum"
  }

  private def typeOfProtectionUAEvent8(tOP: String): String = tOP match {
    case "Primary Protection" => "primaryProtection"
    case "Enhanced protection" => "enhancedProtection"
  }

  private def typeOfProtectionUAEvent8A(tOP: String): String = tOP match {
    case "Primary Protection" => "primaryProtection"
    case "Enhanced protection" => "enhancedProtection"
  }

  private def reasonBenefitTakenUAEvent3(rBT: String): String = rBT match {
    case "Ill Health" => "illHealth"
    case "Protected Pension Age" => "protectedPensionAge"
    case "Other" => "other"
  }

  private def typeOfProtectionUAEvent24(tOP: String): String = tOP match {
    case "Enhanced protection" => "enhancedProtection"
    case "Enhanced protection with protected lump sum rights of more than 375,000" => "enhancedProtectionWithProtectedSum"
    case "Fixed protection" => "fixedProtection"
    case "Fixed protection 2014" => "fixedProtection2014"
    case "Fixed protection 2016" => "fixedProtection2016"
    case "Individual protection 2014" => "individualProtection2014"
    case "Individual protection 2016" => "individualProtection2016"
    case "Primary Protection" => "primaryProtection"
    case "Primary protection with protected lump sum rights of more than 375,000" => "primaryWithProtectedSum"
  }

  private def bceTypeUAEvent24(tOP: String): String = tOP match {
    case "An annuity protection lump sum death benefit" => "annuityProtection"
    case "A defined benefit lump sum death benefit" => "definedBenefit"
    case "A drawdown pension fund lump sum death benefit" => "drawdown"
    case "A flexi-access drawdown lump sum death benefit" => "flexiAccess"
    case "A pension protection lump sum death benefit" => "pensionProtection"
    case "A small lump sum" => "small"
    case "A stand-alone lump sum" => "standAlone"
    case "A trivial commutation lump sum" => "trivialCommutation"
    case "Serious ill health lump sum" => "seriousHealthLumpSum"
    case "An uncrystalised funds pension lump sum" => "uncrystallisedFunds"
    case "A uncrystallised funds lump sum death benefit" => "uncrystallisedFundsDeathBenefit"
    case "A winding-up lump sum" => "windingUp"
  }
}

//noinspection ScalaStyle
private object MemberEventReportPaths {

  /* UserAnswers paths in alphabetical order */
  def pathUaEventDetailsForEventType(eventType: EventType): JsPath = __ \ Symbol(s"event${eventType.toString}") \ Symbol("members")

  // UA - nested once
  val pathUaAmountPaid: JsPath = __ \ Symbol("amountPaid")
  val pathUaMemberStatus: JsPath = __ \ Symbol("memberStatus")
  val pathUaAmendedVersion: JsPath = __ \ Symbol("amendedVersion")
  val pathUaAmountCrystalised: JsPath = __ \ Symbol("totalAmountBenefitCrystallisation")
  val pathUaAvailableLumpSumExceeded: JsPath = __ \ Symbol("overAllowance")
  val pathUaAvailableLumpSumDBAExceeded: JsPath = __ \ Symbol("overAllowanceAndDeathBenefit")
  val pathUaBCEType: JsPath = __ \ Symbol("bceTypeSelection")
  val pathUaBeneficiaryDetails: JsPath = __ \ Symbol("beneficiaryDetails")
  val pathUaChooseTaxYearEvent: JsPath = __ \ Symbol("chooseTaxYear")
  val pathUaCrystallisedAmount: JsPath = __ \ Symbol("crystallisedAmount")
  val pathUaDatePaid: JsPath = __ \ Symbol("datePaid")
  val pathUaDeceasedMembersDetails: JsPath = __ \ Symbol("deceasedMembersDetails")
  val pathUaFreeTextEvent24: JsPath = __ \ Symbol("typeOfProtectionGroup2Reference")
  val pathUaInputProtectionType: JsPath = __ \ Symbol("inputProtectionType")
  val pathUaLumpSumAmount: JsPath = __ \ Symbol("lumpSumAmount")
  val pathUaMarginalRate: JsPath = __ \ Symbol("marginalRate")
  val pathUaMemberHoldProtection: JsPath = __ \ Symbol("validProtection")
  val pathUaMembersDetails: JsPath = __ \ Symbol("membersDetails")
  val pathUaPayeReference: JsPath = __ \ Symbol("employerPayeReference")
  val pathUaPaymentType: JsPath = __ \ Symbol("paymentType")
  val pathUaProtectionGroup1Event24: JsPath = __ \ Symbol("typeOfProtectionGroup1")
  val pathUaProtectionRefGroup1: JsPath = __ \ Symbol("typeOfProtectionGroup1Reference")
  val pathUaTotalPensionAmounts: JsPath = __ \ Symbol("totalPensionAmounts")
  val pathUaTypeOfProtection: JsPath = __ \ Symbol("typeOfProtection")
  val pathUaTypeOfProtectionEvent24: JsPath = __ \ Symbol("typeOfProtectionGroup2")
  val pathUaTypeOfProtectionReference: JsPath = __ \ Symbol("typeOfProtectionReference")

  // UA - nested twice
  val pathUaPreCommenceReference: JsPath = pathUaProtectionRefGroup1 \ Symbol("preCommencement")
  val pathUaPensionCreditReference: JsPath = pathUaProtectionRefGroup1 \ Symbol("pensionCreditsPreCRE")
  val pathUaNonResidenceReference: JsPath = pathUaProtectionRefGroup1 \ Symbol("nonResidenceEnhancement")
  val pathUaOverseasReference: JsPath = pathUaProtectionRefGroup1 \ Symbol("recognisedOverseasPSTE")
  val pathUaAmountCrystallised: JsPath = __ \ Symbol("AmountCrystallisedAndDate") \ Symbol("amountCrystallised")
  val pathUaCrystallisedDate: JsPath = __ \ Symbol("AmountCrystallisedAndDate") \ Symbol("crystallisedDate")
  val pathUaFreeText: JsPath = __ \ Symbol("benefitType") \ Symbol("freeText")
  val pathUaReasonBenefitTaken: JsPath = __ \ Symbol("benefitType") \ Symbol("reasonBenefitTaken")
  val pathUaLumpSumAmountNested: JsPath = __ \ Symbol("lumpSumAmountAndDate") \ Symbol("lumpSumAmount")
  val pathUaLumpSumDateNested: JsPath = __ \ Symbol("lumpSumAmountAndDate") \ Symbol("lumpSumDate")
  val pathUaPaymentDate: JsPath = __ \ Symbol("paymentDate") \ Symbol("date")
  val pathUaAmountPaidNested: JsPath = __ \ Symbol("paymentDetails") \ Symbol("amountPaid")
  val pathUaEventDate: JsPath = __ \ Symbol("paymentDetails") \ Symbol("eventDate")
  val pathUaCrystallisedDateEvent24: JsPath = __ \ Symbol("crystallisedDate") \ Symbol("date")

  /* ETMP paths in alphabetical order */

  // ETMP - nested once or utils
  val pathEtmlAmendedVersion: JsPath = __ \ Symbol("memberDetail") \ Symbol("amendedVersion")
  val pathEtmpEventDetails: JsPath = __ \ Symbol("eventDetails")
  val pathEtmlMemberStatus: JsPath = __ \ Symbol("memberDetail") \ Symbol("memberStatus")

  private val pathEtmpMemberDetailEvent: JsPath = __ \ Symbol("memberDetail") \ Symbol("event")
  private val pathEtmpMemberDetailEventPaymentDetails: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails")

  // ETMP - nested from "memberDetail" \ "event"
  val pathEtmpIndividualDetails: JsPath = pathEtmpMemberDetailEvent \ Symbol("individualDetails")
  val pathEtmpPersonReceivedThePayment: JsPath = pathEtmpMemberDetailEvent \ Symbol("personReceivedThePayment")

  // ETMP - nested from "memberDetail" \ "event" \ "paymentDetails"
  val pathEtmpAmountBenefit: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("amountBenefit")
  val pathEtmpAmountCrystalised: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("amountCrystalised")
  val pathEtmpAmountLumpSum: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("amountLumpSum")
  val pathEtmpAmountPaid: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("amountPaid")
  val pathEtmpAnnualRate: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("annualRate")
  val pathEtmpAvailableLumpSumExceeded: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("availableLumpSumExceeded")
  val pathEtmpAvailableLumpSumDBAExceeded: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("availableLumpSumDBAExceeded")
  val pathEtmpBCEType: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("reasonBenefitTaken")
  val pathEtmpEventDate: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("eventDate")
  val pathEtmpFreeText: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("freeText")
  val pathEtmpMarginalRate: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("taxedAtMarginalRate")
  val pathEtmpMemberHoldProtectionEvent24: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("memberHoldProtection")
  val pathEtmpMonetaryAmount: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("monetaryAmount")
  val pathEtmpNonResidenceReference: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("nonResidenceReference")
  val pathEtmpOverseasReference: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("overseasReference")
  val pathEtmpPayeReference: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("payeReference")
  val pathEtmpPensionCreditReference: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("pensionCreditReference")
  val pathEtmpPreCommenceReference: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("preCommenceReference")
  val pathEtmpReasonBenefitTaken: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("reasonBenefitTaken")
  val pathEtmpSchemeSpecificLumpSum: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("schemeSpecificLumpSum")
  val pathEtmpTaxYearEndingDate: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("taxYearEndingDate")
  val pathEtmpTypeOfProtection: JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("typeOfProtection")
}
