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

import models.enumeration.EventType
import models.enumeration.EventType.{Event2, Event24, Event3, Event4, Event5, Event6, Event7, Event8, Event8A}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer

object API1830 extends Transformer {

  def transformToETMPData(eventType: EventType, pstr: String): Reads[JsObject] = {
    val extraFieldsForHeaderReads = Reads.pure(
      Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> pstr,
          "eventType" -> s"Event${eventType.toString}"
        )
      )
    )



    HeaderForAllAPIs.transformToETMPData(extraFieldsForHeaderReads).flatMap { hdr =>
      val fullHdr = hdr
      (__ \ Symbol(s"event${eventType.toString}") \ Symbol("members")).readNullable[JsArray](__.read(Reads.seq(
        readsIndividualMemberDetailsByEventType(eventType)))
        .map(JsArray(_))).map { optionJsArray =>

        val eventDetails = optionJsArray match {
          case Some(array) if array.value.isEmpty => Json.obj()
          case Some(array) => Json.obj(
            "eventDetails" -> array
          )
          case None => Json.obj()
        }

        Json.obj("memberEventsDetails" -> (eventDetails ++ fullHdr))
      }
    }
  }

  private def readsIndividualMemberDetailsByEventType(eventType: EventType): Reads[JsObject] = {
    val details = eventType match {
      case Event2 => readsDeceasedAndBeneficiaryMemberDetailsEvent2
      case Event3 => readsIndividualMemberDetailsEvent3
      case Event4 => readsIndividualMemberDetailsEvent4
      case Event5 => readsIndividualMemberDetailsEvent5
      case Event6 => readsIndividualMemberDetailsEvent6
      case Event7 => readsIndividualMemberDetailsEvent7
      case Event8 => readsIndividualMemberDetailsEvent8
      case Event8A => readsIndividualMemberDetailsEvent8A
      case Event24 => readsIndividualMemberDetailsEvent24
      case _ => readsIndividualMemberDetailsEvent22And23(eventType)
    }
    (memberChangeStatusReads and details).reduce
  }

  private def readsDeceasedAndBeneficiaryMemberDetailsEvent2: Reads[JsObject] = {

    val pathPersonReceivedThePayment = pathToEvent \ Symbol("personReceivedThePayment")
    (
      (pathToEvent \ Symbol("eventType")).json.put(JsString(s"Event$Event2")) and
        (pathIndividualMemberDetails \ Symbol("firstName")).json.copyFrom((pathDeceasedMemberDetails \ Symbol("firstName")).json.pick) and
        (pathIndividualMemberDetails \ Symbol("lastName")).json.copyFrom((pathDeceasedMemberDetails \ Symbol("lastName")).json.pick) and
        (pathIndividualMemberDetails \ Symbol("nino")).json.copyFrom((pathDeceasedMemberDetails \ Symbol("nino")).json.pick) and
        (pathPersonReceivedThePayment \ Symbol("firstName")).json.copyFrom((pathBeneficiaryMemberDetails \ Symbol("firstName")).json.pick) and
        (pathPersonReceivedThePayment \ Symbol("lastName")).json.copyFrom((pathBeneficiaryMemberDetails \ Symbol("lastName")).json.pick) and
        (pathPersonReceivedThePayment \ Symbol("nino")).json.copyFrom((pathBeneficiaryMemberDetails \ Symbol("nino")).json.pick) and
        (pathPaymentDetails \ Symbol("amountPaid")).json.copyFrom((__ \ Symbol("amountPaid")).json.pick) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((__ \ Symbol("datePaid")).json.pick)
      ).reduce
  }

  private def readsIndividualMemberDetailsEvent3: Reads[JsObject] = {


    (
      (pathToEvent \ Symbol("eventType")).json.put(JsString(s"Event$Event3")) and
        readsIndividualMemberDetails and
        (pathPaymentDetails \ Symbol("reasonBenefitTaken")).json.copyFrom(readsTypeOfBenefitEvent3) and
        (pathPaymentDetails \ Symbol("amountBenefit")).json.copyFrom((__ \ Symbol("paymentDetails") \ Symbol("amountPaid")).json.pick) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((__ \ Symbol("paymentDetails") \ Symbol("eventDate")).json.pick) and
        ((pathPaymentDetails \ Symbol("freeText")).json.copyFrom((pathBenefitType \ Symbol("freeText")).json.pick) orElse doNothing)
      ).reduce
  }

  private def readsIndividualMemberDetailsEvent4: Reads[JsObject] = {

    (
      (pathToEvent \ Symbol("eventType")).json.put(JsString(s"Event$Event4")) and
        readsIndividualMemberDetails and
        (pathPaymentDetails \ Symbol("amountPaid")).json.copyFrom((__ \ Symbol("paymentDetails") \ Symbol("amountPaid")).json.pick) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((__ \ Symbol("paymentDetails") \ Symbol("eventDate")).json.pick)
      ).reduce
  }

  private def readsIndividualMemberDetailsEvent5: Reads[JsObject] = {
    (
      (pathToEvent \ Symbol("eventType")).json.put(JsString(s"Event$Event5")) and
        readsIndividualMemberDetails and
        (pathPaymentDetails \ Symbol("annualRate")).json.copyFrom((__ \ Symbol("paymentDetails") \ Symbol("amountPaid")).json.pick) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((__ \ Symbol("paymentDetails") \ Symbol("eventDate")).json.pick)
      ).reduce
  }

  private def readsIndividualMemberDetailsEvent6: Reads[JsObject] = {

    (
      (pathToEvent \ Symbol("eventType")).json.put(JsString(s"Event$Event6")) and
        readsIndividualMemberDetails and
        (pathPaymentDetails \ Symbol("amountCrystalised")).json.copyFrom((pathAmountCrystallisedAndDateDetails \ Symbol("amountCrystallised")).json.pick) and
        (pathPaymentDetails \ Symbol("typeOfProtection")).json.copyFrom(readsTypeOfProtectionEvent6) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((pathAmountCrystallisedAndDateDetails \ Symbol("crystallisedDate")).json.pick) and
        (pathPaymentDetails \ Symbol("freeText")).json.copyFrom((__ \ Symbol("inputProtectionType")).json.pick) and
        pathMemberStatus.json.copyFrom((__ \ Symbol("memberStatus")).json.pick)
      ).reduce
  }

  private def readsIndividualMemberDetailsEvent7: Reads[JsObject] = {

    (
      (pathToEvent \ Symbol("eventType")).json.put(JsString(s"Event$Event7")) and
        readsIndividualMemberDetails and
        (pathPaymentDetails \ Symbol("amountLumpSum")).json.copyFrom((__ \ Symbol("lumpSumAmount")).json.pick) and
        (pathPaymentDetails \ Symbol("amountCrystalised")).json.copyFrom((__ \ Symbol("crystallisedAmount")).json.pick) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((__ \ Symbol("paymentDate") \ Symbol("date")).json.pick)
      ).reduce
  }

  private def readsIndividualMemberDetailsEvent8: Reads[JsObject] = {
    (
      (pathToEvent \ Symbol("eventType")).json.put(JsString(s"Event$Event8")) and
        readsIndividualMemberDetails and
        (pathPaymentDetails \ Symbol("amountLumpSum")).json.copyFrom((pathLumpSumAmountAndDateDetails \ Symbol("lumpSumAmount")).json.pick) and
        (pathPaymentDetails \ Symbol("typeOfProtection")).json.copyFrom(readsTypeOfProtectionEvent8) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((pathLumpSumAmountAndDateDetails \ Symbol("lumpSumDate")).json.pick) and
        (pathPaymentDetails \ Symbol("freeText")).json.copyFrom((__ \ Symbol("typeOfProtectionReference")).json.pick)
      ).reduce
  }

  private def readsIndividualMemberDetailsEvent8A: Reads[JsObject] = {
   (
      (pathToEvent \ Symbol("eventType")).json.put(JsString(s"Event$Event8A")) and
        readsIndividualMemberDetails and
        (pathPaymentDetails \ Symbol("reasonBenefitTaken")).json.copyFrom(readsPaymentTypeEvent8A) and
        ((pathPaymentDetails \ Symbol("typeOfProtection")).json.copyFrom(readsTypeOfProtectionEvent8A) orElse doNothing) and
        (pathPaymentDetails \ Symbol("amountLumpSum")).json.copyFrom((pathLumpSumAmountAndDateDetails \ Symbol("lumpSumAmount")).json.pick) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((pathLumpSumAmountAndDateDetails \ Symbol("lumpSumDate")).json.pick) and
        ((pathPaymentDetails \ Symbol("freeText")).json.copyFrom((__ \ Symbol("typeOfProtectionReference")).json.pick) orElse doNothing)
     ).reduce
  }

  private def readsIndividualMemberDetailsEvent24: Reads[JsObject] = {
    (
      (pathToEvent \ Symbol("eventType")).json.put(JsString(s"Event$Event24")) and
        readsIndividualMemberDetails and
        (pathPaymentDetails \ Symbol("memberHoldProtection")).json.copyFrom(readsMemberHoldProtection) and
        (pathPaymentDetails \ Symbol("preCommenceReference"))
          .json.copyFrom(readsPreCommenceReference).orElse(doNothing) and
        (pathPaymentDetails \ Symbol("pensionCreditReference"))
          .json.copyFrom(readsPensionCreditReference).orElse(doNothing) and
        (pathPaymentDetails \ Symbol("nonResidenceReference"))
          .json.copyFrom(readsNonResidenceEnhancement).orElse(doNothing) and
        (pathPaymentDetails \ Symbol("overseasReference"))
          .json.copyFrom(readsOverseasReference).orElse(doNothing) and
        (pathPaymentDetails \ Symbol("availableLumpSumDBAExceeded")).json.copyFrom(readsAvailableLumpSumDBAExceeded) and
        (pathPaymentDetails \ Symbol("availableLumpSumExceeded")).json.copyFrom(readsAvailableLumpSumExceeded) and
        (pathPaymentDetails \ Symbol("schemeSpecificLumpSum")).json.copyFrom(readsSchemeSpecificLumpSum).orElse(doNothing) and
        (pathPaymentDetails \ Symbol("amountCrystalised")).json.copyFrom((__ \ Symbol("totalAmountBenefitCrystallisation")).json.pick) and
        (pathPaymentDetails \ Symbol("typeOfProtection")).json.copyFrom(readsTypeOfProtectionGroup2Event24).orElse(doNothing) and
        (pathPaymentDetails \ Symbol("reasonBenefitTaken")).json.copyFrom(readsReasonBenefitTakenEvent24) and
        (pathPaymentDetails \ Symbol("taxYearEndingDate")).json.copyFrom(pathTaxYearEndingDateEvent24.json.pick) and
        (pathPaymentDetails \ Symbol("freeText")).json.copyFrom((__ \ Symbol("typeOfProtectionGroup2Reference")).json.pick).orElse(doNothing) and
        (pathPaymentDetails \ Symbol("taxedAtMarginalRate")).json.copyFrom(readsTaxedAtMarginalRate).orElse(doNothing) and
        (pathPaymentDetails \ Symbol("payeReference")).json.copyFrom((__ \ Symbol("employerPayeReference")).json.pick).orElse(doNothing)
      ).reduce
  }

  private def readsIndividualMemberDetailsEvent22And23(eventType: EventType): Reads[JsObject] = {

    (
      (pathToEvent \ Symbol("eventType")).json.put(JsString(s"Event${eventType}")) and
        readsIndividualMemberDetails and
        (pathPaymentDetails \ Symbol("taxYearEndingDate")).json.copyFrom(readsTaxYearEndDate) and
        (pathPaymentDetails \ Symbol("monetaryAmount")).json.copyFrom((__ \ Symbol("totalPensionAmounts")).json.pick)
      ).reduce
  }

  private val pathBenefitType = __ \ Symbol("benefitType")
  private val pathAmountCrystallisedAndDateDetails = __ \ Symbol("AmountCrystallisedAndDate")
  private val pathLumpSumAmountAndDateDetails = __ \ Symbol("lumpSumAmountAndDate")

  private val pathDeceasedMemberDetails = __ \ Symbol("deceasedMembersDetails")
  private val pathBeneficiaryMemberDetails = __ \ Symbol("beneficiaryDetails")

  private val pathCrystallisedDateEvent24 = __ \ Symbol("crystallisedDate")
  private val pathTaxYearEndingDateEvent24 = pathCrystallisedDateEvent24 \ Symbol("date")

  private val readsTaxYearEndDate: Reads[JsString] = (__ \ Symbol("chooseTaxYear")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(s"${str.toInt + 1}-04-05"))
    case _ => fail(JsString("taxYearEndDate"))
  }

  private val readsTypeOfBenefitEvent3: Reads[JsString] = (pathBenefitType \ Symbol("reasonBenefitTaken")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event3TypeOfBenefitConversion(str)))
    case _ => fail(JsString("typeOfBenefitEvent3"))
  }

  private val readsTypeOfProtectionEvent6: Reads[JsString] = (__ \ Symbol("typeOfProtection")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event6TypeOfProtectionConversion(str)))
    case _ => fail(JsString("typeOfProtectionEvent6"))
  }

  private val readsTypeOfProtectionEvent8: Reads[JsString] = (__ \ Symbol("typeOfProtection")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event8TypeOfProtectionConversion(str)))
    case _ => fail(JsString("typeOfProtectionEvent8"))
  }

  private val readsTypeOfProtectionEvent8A: Reads[JsString] = (__ \ Symbol("typeOfProtection")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event8ATypeOfProtectionConversion(str)))
    case _ => fail(JsString("typeOfProtectionEvent8A"))
  }

  private val readsPaymentTypeEvent8A: Reads[JsString] = (__ \ Symbol("paymentType")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event8APaymentTypeConversion(str)))
    case _ => fail(JsString("paymentTypeEvent8A"))
  }

  private val readsMemberHoldProtection: Reads[JsString] = (__ \ Symbol("validProtection")).json.pick.flatMap {
    case JsBoolean(value) => Reads.pure(toYesNo(JsBoolean(value)))
    case _ => fail(JsString("memberHoldProtection"))
  }

  private val readsAvailableLumpSumExceeded: Reads[JsString] = (__ \ Symbol("overAllowanceAndDeathBenefit")).json.pick.flatMap {
    case JsBoolean(value) => value match {
      case true => Reads.pure(toYesNo(JsBoolean(false)))
      case false => (__ \ Symbol("overAllowance")).json.pick.flatMap {
        case JsBoolean(value) => Reads.pure(toYesNo(JsBoolean(value)))
        case _ => fail(JsString("availableLumpSumExceeded"))
      }
    }
    case _ => fail(JsString("availableLumpSumDBAExceeded"))
  }

  private val readsAvailableLumpSumDBAExceeded: Reads[JsString] = (__ \ Symbol("overAllowanceAndDeathBenefit")).json.pick.flatMap {
    case JsBoolean(value) => Reads.pure(toYesNo(JsBoolean(value)))
    case _ => fail(JsString("availableLumpSumDBAExceeded"))
  }

  private val readsSchemeSpecificLumpSum: Reads[JsString] = (__ \ Symbol("typeOfProtectionGroup1")).json.pick.flatMap {
    case JsArray(value) if value.contains(JsString("schemeSpecific")) => Reads.pure(JsString("Yes"))
    case _ => fail(JsString("schemeSpecificLumpSum"))
  }

  private val readsTypeOfProtectionGroup2Event24: Reads[JsString] = (__ \ Symbol("typeOfProtectionGroup2")).json.pick.flatMap {
    case JsString(str) if str != "noOtherProtections" => Reads.pure(JsString(event24TypeOfProtectionGroup2Conversion(str)))
    case _ => fail(JsString("typeOfProtectionGroup2Event24"))
  }

  private val readsReasonBenefitTakenEvent24: Reads[JsString] = (__ \ Symbol("bceTypeSelection")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event24ReasonBenefitTakenConversion(str)))
    case _ => fail(JsString("reasonBenefitTakenEvent24"))
  }

  private val readsTaxedAtMarginalRate: Reads[JsString] = (__ \ Symbol("marginalRate")).json.pick.flatMap {
    case JsBoolean(value) => Reads.pure(toYesNo(JsBoolean(value)))
    case _ => fail(JsString("taxedAtMarginalRate"))
  }

  private val pathToEvent: JsPath = __ \ Symbol("memberDetail") \ Symbol("event")
  private val pathIndividualMemberDetails: JsPath = pathToEvent \ Symbol("individualDetails")
  private val pathMemberStatus: JsPath = __ \ Symbol("memberDetail") \ Symbol("memberStatus")
  private val pathAmendedVersion: JsPath = __ \ Symbol("memberDetail") \ Symbol("amendedVersion")
  private val pathPaymentDetails = pathToEvent \ Symbol("paymentDetails")
  private val pathToTypeOfProtectionReferenceGroup1 = __ \ Symbol("typeOfProtectionGroup1Reference")
  private val pathToNonResidenceEnhancement = pathToTypeOfProtectionReferenceGroup1 \ Symbol("nonResidenceEnhancement")
  private val pathToPreCommenceReference = pathToTypeOfProtectionReferenceGroup1 \ Symbol("preCommencement")
  private val pathToOverseasReference = pathToTypeOfProtectionReferenceGroup1 \ Symbol("recognisedOverseasPSTE")
  private val pathToPensionCreditReference = pathToTypeOfProtectionReferenceGroup1 \ Symbol("pensionCreditsPreCRE")

  private val readsNonResidenceEnhancement: Reads[JsString] = pathToNonResidenceEnhancement.json.pick.flatMap {
    case JsString(value) if value.nonEmpty => Reads.pure(JsString(value))
    case _ => fail(JsString("nonResidenceEnhancement"))
  }

  private val readsPreCommenceReference: Reads[JsString] = pathToPreCommenceReference.json.pick.flatMap {
    case JsString(value) if value.nonEmpty => Reads.pure(JsString(value))
    case _ => fail(JsString("preCommenceReference"))
  }

  private val readsOverseasReference: Reads[JsString] = pathToOverseasReference.json.pick.flatMap {
    case JsString(value) if value.nonEmpty => Reads.pure(JsString(value))
    case _ => fail(JsString("overseasReference"))
  }

  private val readsPensionCreditReference: Reads[JsString] = pathToPensionCreditReference.json.pick.flatMap {
    case JsString(value) if value.nonEmpty => Reads.pure(JsString(value))
    case _ => fail(JsString("pensionCreditReference"))
  }

  private val memberChangeStatusReads = (
    pathMemberStatus.json.copyFrom((__ \ Symbol("memberStatus")).json.pick).orElse(doNothing) and
      pathAmendedVersion.json.copyFrom((__ \ Symbol("amendedVersion")).json.pick).orElse(doNothing)
    ).reduce

  private val readsIndividualMemberDetails: Reads[JsObject] = {
    (
      (pathIndividualMemberDetails \ Symbol("firstName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("firstName")).json.pick) and
        (pathIndividualMemberDetails \ Symbol("lastName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("lastName")).json.pick) and
        (pathIndividualMemberDetails \ Symbol("nino")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("nino")).json.pick)
      ).reduce
  }

  private def event3TypeOfBenefitConversion(tOB: String): String = tOB match {
    case "illHealth" => "Ill Health"
    case "protectedPensionAge" => "Protected Pension Age"
    case "other" => "Other"
  }

  private def event6TypeOfProtectionConversion(tOP: String): String = tOP match {
    case "enhancedLifetimeAllowance" => "Enhanced life time allowance"
    case "enhancedProtection" => "Enhanced protection"
    case "fixedProtection" => "Fixed protection"
    case "fixedProtection2014" => "Fixed protection 2014"
    case "fixedProtection2016" => "Fixed protection 2016"
    case "individualProtection2014" => "Individual protection 2014"
    case "individualProtection2016" => "Individual protection 2016"
  }

  private def event8TypeOfProtectionConversion(tOP: String): String = tOP match {
    case "primaryProtection" => "Primary Protection"
    case "enhancedProtection" => "Enhanced protection"
  }

  private def event8ATypeOfProtectionConversion(tOP: String): String = tOP match {
    case "primaryProtection" => "Primary Protection"
    case "enhancedProtection" => "Enhanced protection"
  }

  //noinspection ScalaStyle
  private def event8APaymentTypeConversion(pT: String): String = pT match {
    case "paymentOfAStandAloneLumpSum" =>
      "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection"
    case "paymentOfASchemeSpecificLumpSum" =>
      "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance"
  }

  private def event24TypeOfProtectionGroup2Conversion(tOP: String): String = tOP match {
      case "enhancedProtection" => "Enhanced protection"
      case "enhancedProtectionWithProtectedSum" => "Enhanced protection with protected lump sum rights of more than 375,000"
      case "fixedProtection" => "Fixed protection"
      case "fixedProtection2014" => "Fixed protection 2014"
      case "fixedProtection2016" => "Fixed protection 2016"
      case "individualProtection2014" => "Individual protection 2014"
      case "individualProtection2016" => "Individual protection 2016"
      case "primary" => "Primary Protection"
      case "primaryWithProtectedSum" => "Primary protection with protected lump sum rights of more than 375,000"
  }

  //noinspection ScalaStyle
  private def event24ReasonBenefitTakenConversion(tOB: String): String = tOB match {
    case "annuityProtection" => "An annuity protection lump sum death benefit"
    case "definedBenefit" => "A defined benefit lump sum death benefit"
    case "drawdown" => "A drawdown pension fund lump sum death benefit"
    case "flexiAccess" => "A flexi-access drawdown lump sum death benefit"
    case "commencement" => "Pension commencement lump sum"
    case "pensionProtection" => "A pension protection lump sum death benefit"
    case "small" => "A small lump sum"
    case "standAlone" => "A stand-alone lump sum"
    case "trivialCommutation" => "A trivial commutation lump sum"
    case "seriousHealthLumpSum" => "Serious ill health lump sum"
    case "uncrystallisedFunds" => "An uncrystalised funds pension lump sum"
    case "uncrystallisedFundsDeathBenefit" => "A uncrystallised funds lump sum death benefit"
    case "windingUp" => "A winding-up lump sum"
  }
}

