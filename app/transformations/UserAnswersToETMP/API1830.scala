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
  import transformations.UserAnswersToETMP.API1830ReadsUtilities._

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
}

private object API1830ReadsUtilities extends Transformer {
  import transformations.UserAnswersToETMP.API1830Paths._
  def readsIndividualMemberDetailsByEventType(eventType: EventType): Reads[JsObject] = {
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

  private def readsDeceasedAndBeneficiaryMemberDetailsEvent2: Reads[JsObject] = (
    etmpPathToEventType.json.put(JsString(s"Event$Event2")) and
      reqReads(etmpPathToIndividualMemberFirstName, uaPathToDeceasedMemberFirstName) and
      reqReads(etmpPathToIndividualMemberLastName, uaPathToDeceasedMemberLastName) and
      reqReads(etmpPathToIndividualMemberNino, uaPathToDeceasedMemberNino) and
      reqReads(etmpPathToPersonReceivedThePaymentFirstName, uaPathToBeneficiaryFirstName) and
      reqReads(etmpPathToPersonReceivedThePaymentLastName, uaPathToBeneficiaryLastName) and
      reqReads(etmpPathToPersonReceivedThePaymentNino, uaPathToBeneficiaryNino) and
      reqReads(etmpPathToAmountPaid, uaPathToAmountPaid) and
      reqReads(etmpPathToEventDate, uaPathToDatePaid)
    ).reduce

  private def readsIndividualMemberDetailsEvent3: Reads[JsObject] = (
    etmpPathToEventType.json.put(JsString(s"Event$Event3")) and
      readsIndividualMemberDetails and
      reqNestedReadsJsString(etmpPathToReasonBenefitTaken, readsTypeOfBenefitEvent3) and
      reqReads(etmpPathToAmountBenefit, uaPathToAmountPaidFromPaymentDetails) and
      reqReads(etmpPathToEventDate, uaPathToEventDate) and
      optReads(etmpPathToFreeText, uaPathToFreeText)
    ).reduce

  private def readsIndividualMemberDetailsEvent4: Reads[JsObject] = (
    etmpPathToEventType.json.put(JsString(s"Event$Event4")) and
      readsIndividualMemberDetails and
      etmpPathToAmountPaid.json.copyFrom(uaPathToAmountPaidFromPaymentDetails.json.pick) and
      etmpPathToEventDate.json.copyFrom(uaPathToEventDate.json.pick)
    ).reduce

  private def readsIndividualMemberDetailsEvent5: Reads[JsObject] = (
    etmpPathToEventType.json.put(JsString(s"Event$Event5")) and
      readsIndividualMemberDetails and
      reqReads(etmpPathToAnnualRate, uaPathToAmountPaidFromPaymentDetails) and
      reqReads(etmpPathToEventDate, uaPathToEventDate)
    ).reduce

  private def readsIndividualMemberDetailsEvent6: Reads[JsObject] = (
    etmpPathToEventType.json.put(JsString(s"Event$Event6")) and
      readsIndividualMemberDetails and
      reqReads(etmpPathToAmountCrystalised, uaPathToAmountCrystallisedFromDetails) and
      reqNestedReadsJsString(etmpPathToTypeOfProtection, readsTypeOfProtectionEvent6) and
      reqReads(etmpPathToEventDate, uaPathToCrystallisedDateFromDetails) and
      reqReads(etmpPathToFreeText, uaPathToInputProtectionType) and
      reqReads(etmpPathToMemberStatus, uaPathToMemberStatus)
    ).reduce

  private def readsIndividualMemberDetailsEvent7: Reads[JsObject] = (
    etmpPathToEventType.json.put(JsString(s"Event$Event7")) and
      readsIndividualMemberDetails and
      reqReads(etmpPathToAmountLumpSum, uaPathToLumpSumAmount) and
      reqReads(etmpPathToAmountCrystalised, uaPathToCrystallisedAmount) and
      reqReads(etmpPathToEventDate, uaPathToPaymentDate)
    ).reduce

  private def readsIndividualMemberDetailsEvent8: Reads[JsObject] = (
    etmpPathToEventType.json.put(JsString(s"Event$Event8")) and
      readsIndividualMemberDetails and
      reqReads(etmpPathToAmountLumpSum, uaPathToLumpSumAmountFromDetails) and
      reqNestedReadsJsString(etmpPathToTypeOfProtection, readsTypeOfProtectionEvent8) and
      reqReads(etmpPathToEventDate, uaPathToLumpSumDateFromDetails) and
      reqReads(etmpPathToFreeText, uaPathToTypeOfProtectionReference)
    ).reduce

  private def readsIndividualMemberDetailsEvent8A: Reads[JsObject] = (
    etmpPathToEventType.json.put(JsString(s"Event$Event8A")) and
      readsIndividualMemberDetails and
      reqNestedReadsJsString(etmpPathToReasonBenefitTaken, readsPaymentTypeEvent8A) and
      optNestedReadsJsString(etmpPathToTypeOfProtection, readsTypeOfProtectionEvent8A) and
      reqReads(etmpPathToAmountLumpSum, uaPathToLumpSumAmountFromDetails) and
      reqReads(etmpPathToEventDate, uaPathToLumpSumDateFromDetails) and
      optReads(etmpPathToFreeText, uaPathToTypeOfProtectionReference)
    ).reduce

  private def readsIndividualMemberDetailsEvent24: Reads[JsObject] = (
    etmpPathToEventType.json.put(JsString(s"Event$Event24")) and
      readsIndividualMemberDetails and
      reqNestedReadsJsString(etmpPathToMemberHoldProtection, readsMemberHoldProtection) and
      optNestedReadsJsString(etmpPathToPreCommenceReference, readsPreCommenceReference) and
      optNestedReadsJsString(etmpPathToPensionCreditReference, readsPensionCreditReference) and
      optNestedReadsJsString(etmpPathToNonResidenceReference, readsNonResidenceEnhancement) and
      optNestedReadsJsString(etmpPathToOverseasReference, readsOverseasReference) and
      reqNestedReadsJsString(etmpPathToAvailableLumpSumDBAExceeded, readsAvailableLumpSumDBAExceeded) and
      reqNestedReadsJsString(etmpPathToAvailableLumpSumExceeded, readsAvailableLumpSumExceeded) and
      optNestedReadsJsString(etmpPathToSchemeSpecificLumpSum, readsSchemeSpecificLumpSum) and
      reqReads(etmpPathToAmountCrystalised, uaPathToTotalAmountBenefitCrystallisation) and
      optNestedReadsJsString(etmpPathToTypeOfProtection, readsTypeOfProtectionGroup2Event24) and
      reqNestedReadsJsString(etmpPathToReasonBenefitTaken, readsReasonBenefitTakenEvent24) and
      reqReads(etmpPathToTaxYearEndingDate, uaPathToTaxYearEndingDateEvent24) and
      optReads(etmpPathToFreeText, uaPathToTypeOfProtectionGroup2Reference) and
      optNestedReadsJsString(etmpPathToTaxedAtMarginalRate, readsTaxedAtMarginalRate) and
      optReads(etmpPathToPayeReference, uaPathToEmployerPayeReference)
    ).reduce

  private def readsIndividualMemberDetailsEvent22And23(eventType: EventType): Reads[JsObject] = (
    etmpPathToEventType.json.put(JsString(s"Event${eventType}")) and
      readsIndividualMemberDetails and
      reqNestedReadsJsString(etmpPathToTaxYearEndingDate, readsTaxYearEndDate) and
      reqReads(etmpPathToMonetaryAmount, uaPathToTotalPensionAmounts)
    ).reduce


  private val readsTaxYearEndDate: Reads[JsString] = uaPathToChooseTaxYear.json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(s"${str.toInt + 1}-04-05"))
    case _ => fail(JsString("taxYearEndDate"))
  }

  private val readsTypeOfBenefitEvent3: Reads[JsString] = uaPathToReasonBenefitTaken.json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event3TypeOfBenefitConversion(str)))
    case _ => fail(JsString("typeOfBenefitEvent3"))
  }

  private val readsTypeOfProtectionEvent6: Reads[JsString] = uaPathToTypeOfProtection.json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event6TypeOfProtectionConversion(str)))
    case _ => fail(JsString("typeOfProtectionEvent6"))
  }

  private val readsTypeOfProtectionEvent8: Reads[JsString] = uaPathToTypeOfProtection.json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event8TypeOfProtectionConversion(str)))
    case _ => fail(JsString("typeOfProtectionEvent8"))
  }

  private val readsTypeOfProtectionEvent8A: Reads[JsString] = uaPathToTypeOfProtection.json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event8ATypeOfProtectionConversion(str)))
    case _ => fail(JsString("typeOfProtectionEvent8A"))
  }

  private val readsPaymentTypeEvent8A: Reads[JsString] = uaPathToPaymentType.json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event8APaymentTypeConversion(str)))
    case _ => fail(JsString("paymentTypeEvent8A"))
  }

  private val readsMemberHoldProtection: Reads[JsString] = uaPathToValidProtection.json.pick.flatMap {
    case JsBoolean(value) => Reads.pure(toYesNo(JsBoolean(value)))
    case _ => fail(JsString("memberHoldProtection"))
  }

  private val readsAvailableLumpSumExceeded: Reads[JsString] = uaPathToOverAllowanceAndDeathBenefit.json.pick.flatMap {
    case JsBoolean(value) => value match {
      case true => Reads.pure(toYesNo(JsBoolean(false)))
      case false => (__ \ Symbol("overAllowance")).json.pick.flatMap {
        case JsBoolean(value) => Reads.pure(toYesNo(JsBoolean(value)))
        case _ => fail(JsString("availableLumpSumExceeded"))
      }
    }
    case _ => fail(JsString("availableLumpSumDBAExceeded"))
  }

  private val readsAvailableLumpSumDBAExceeded: Reads[JsString] = uaPathToOverAllowanceAndDeathBenefit.json.pick.flatMap {
    case JsBoolean(value) => Reads.pure(toYesNo(JsBoolean(value)))
    case _ => fail(JsString("availableLumpSumDBAExceeded"))
  }

  private val readsSchemeSpecificLumpSum: Reads[JsString] = uaPathToTypeOfProtectionGroup1.json.pick.flatMap {
    case JsArray(value) if value.contains(JsString("schemeSpecific")) => Reads.pure(JsString("Yes"))
    case _ => fail(JsString("schemeSpecificLumpSum"))
  }

  private val readsTypeOfProtectionGroup2Event24: Reads[JsString] = uaPathToTypeOfProtectionGroup2.json.pick.flatMap {
    case JsString(str) if str != "noOtherProtections" => Reads.pure(JsString(event24TypeOfProtectionGroup2Conversion(str)))
    case _ => fail(JsString("typeOfProtectionGroup2Event24"))
  }

  private val readsReasonBenefitTakenEvent24: Reads[JsString] = uaPathToBceTypeSelection.json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event24ReasonBenefitTakenConversion(str)))
    case _ => fail(JsString("reasonBenefitTakenEvent24"))
  }

  private val readsTaxedAtMarginalRate: Reads[JsString] = uaPathToMarginalRate.json.pick.flatMap {
    case JsBoolean(value) => Reads.pure(toYesNo(JsBoolean(value)))
    case _ => fail(JsString("taxedAtMarginalRate"))
  }

  private val readsNonResidenceEnhancement: Reads[JsString] = uaPathToNonResidenceEnhancement.json.pick.flatMap {
    case JsString(value) if value.nonEmpty => Reads.pure(JsString(value))
    case _ => fail(JsString("nonResidenceEnhancement"))
  }

  private val readsPreCommenceReference: Reads[JsString] = uaPathToPreCommenceReference.json.pick.flatMap {
    case JsString(value) if value.nonEmpty => Reads.pure(JsString(value))
    case _ => fail(JsString("preCommenceReference"))
  }

  private val readsOverseasReference: Reads[JsString] = uaPathToOverseasReference.json.pick.flatMap {
    case JsString(value) if value.nonEmpty => Reads.pure(JsString(value))
    case _ => fail(JsString("overseasReference"))
  }

  private val readsPensionCreditReference: Reads[JsString] = uaPathToPensionCreditReference.json.pick.flatMap {
    case JsString(value) if value.nonEmpty => Reads.pure(JsString(value))
    case _ => fail(JsString("pensionCreditReference"))
  }

  private val memberChangeStatusReads = (
    optReads(etmpPathToMemberStatus, uaPathToMemberStatus) and
      optReads(etmpPathToAmendedVersion, uaPathToAmendedVersion)
    ).reduce

  private val readsIndividualMemberDetails: Reads[JsObject] = {
    (
      reqReads(etmpPathToIndividualMemberFirstName, uaPathToMembersFirstName) and
        reqReads(etmpPathToIndividualMemberLastName, uaPathToMembersLastName) and
        reqReads(etmpPathToIndividualMemberNino, uaPathToMembersNino)
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

  lazy val reqReads: (JsPath, JsPath) => Reads[JsObject] = (etmpPath: JsPath, uaPath: JsPath) => etmpPath.json.copyFrom(uaPath.json.pick)
  lazy val reqNestedReadsJsString: (JsPath, Reads[JsString]) => Reads[JsObject] =
    (etmpPath: JsPath, uaReads: Reads[JsString]) => etmpPath.json.copyFrom(uaReads)

  lazy val optReads: (JsPath, JsPath) => Reads[JsObject] = (etmpPath: JsPath, uaPath: JsPath) => etmpPath.json.copyFrom(uaPath.json.pick).orElse(doNothing)
  lazy val optNestedReadsJsString: (JsPath, Reads[JsString]) => Reads[JsObject] =
    (etmpPath: JsPath, uaReads: Reads[JsString]) => etmpPath.json.copyFrom(uaReads).orElse(doNothing)
}

private object API1830Paths {
  // ETMP
  private val etmpPathToMemberDetail:                             JsPath = __ \ "memberDetail"
  val etmpPathToAmendedVersion:                                   JsPath = etmpPathToMemberDetail \ "amendedVersion"
  val etmpPathToMemberStatus:                                     JsPath = etmpPathToMemberDetail \ "memberStatus"

  private val etmpPathToEvent:                                    JsPath = etmpPathToMemberDetail \ "event"
  val etmpPathToEventType:                                        JsPath = etmpPathToEvent \ "eventType"

  val etmpPathToIndividualMemberDetails:                          JsPath = etmpPathToEvent \ "individualDetails"
  val etmpPathToIndividualMemberFirstName:                        JsPath = etmpPathToIndividualMemberDetails \ "firstName"
  val etmpPathToIndividualMemberLastName:                         JsPath = etmpPathToIndividualMemberDetails \ "lastName"
  val etmpPathToIndividualMemberNino:                             JsPath = etmpPathToIndividualMemberDetails \ "nino"

  private val etmpPathToPersonReceivedThePayment:                 JsPath = etmpPathToEvent \ "personReceivedThePayment"
  val etmpPathToPersonReceivedThePaymentFirstName:                JsPath = etmpPathToPersonReceivedThePayment \ "firstName"
  val etmpPathToPersonReceivedThePaymentLastName:                 JsPath = etmpPathToPersonReceivedThePayment \ "lastName"
  val etmpPathToPersonReceivedThePaymentNino:                     JsPath = etmpPathToPersonReceivedThePayment \ "nino"

  private val etmpPathToPaymentDetails:                           JsPath = etmpPathToEvent \ "paymentDetails"
  val etmpPathToAmountBenefit:                                    JsPath = etmpPathToPaymentDetails \ "amountBenefit"
  val etmpPathToAmountCrystalised:                                JsPath = etmpPathToPaymentDetails \ "amountCrystalised"
  val etmpPathToAmountLumpSum:                                    JsPath = etmpPathToPaymentDetails \ "amountLumpSum"
  val etmpPathToAmountPaid:                                       JsPath = etmpPathToPaymentDetails \ "amountPaid"
  val etmpPathToAnnualRate:                                       JsPath = etmpPathToPaymentDetails \ "annualRate"
  val etmpPathToAvailableLumpSumDBAExceeded:                      JsPath = etmpPathToPaymentDetails \ "availableLumpSumDBAExceeded"
  val etmpPathToAvailableLumpSumExceeded:                         JsPath = etmpPathToPaymentDetails \ "availableLumpSumExceeded"
  val etmpPathToEventDate:                                        JsPath = etmpPathToPaymentDetails \ "eventDate"
  val etmpPathToFreeText:                                         JsPath = etmpPathToPaymentDetails \ "freeText"
  val etmpPathToMemberHoldProtection:                             JsPath = etmpPathToPaymentDetails \ "memberHoldProtection"
  val etmpPathToMonetaryAmount:                                   JsPath = etmpPathToPaymentDetails \ "monetaryAmount"
  val etmpPathToNonResidenceReference:                            JsPath = etmpPathToPaymentDetails \ "nonResidenceReference"
  val etmpPathToOverseasReference:                                JsPath = etmpPathToPaymentDetails \ "overseasReference"
  val etmpPathToPayeReference:                                    JsPath = etmpPathToPaymentDetails \ "payeReference"
  val etmpPathToPensionCreditReference:                           JsPath = etmpPathToPaymentDetails \ "pensionCreditReference"
  val etmpPathToPreCommenceReference:                             JsPath = etmpPathToPaymentDetails \ "preCommenceReference"
  val etmpPathToReasonBenefitTaken:                               JsPath = etmpPathToPaymentDetails \ "reasonBenefitTaken"
  val etmpPathToSchemeSpecificLumpSum:                            JsPath = etmpPathToPaymentDetails \ "schemeSpecificLumpSum"
  val etmpPathToTaxedAtMarginalRate:                              JsPath = etmpPathToPaymentDetails \ "taxedAtMarginalRate"
  val etmpPathToTaxYearEndingDate:                                JsPath = etmpPathToPaymentDetails \ "taxYearEndingDate"
  val etmpPathToTypeOfProtection:                                 JsPath = etmpPathToPaymentDetails \ "typeOfProtection"



  // UA
  private val uaPathToAmountCrystallisedAndDateDetails:           JsPath = __ \ "AmountCrystallisedAndDate"
  val uaPathToCrystallisedDateFromDetails:                        JsPath = uaPathToAmountCrystallisedAndDateDetails \ "crystallisedDate"
  val uaPathToAmountCrystallisedFromDetails:                      JsPath = uaPathToAmountCrystallisedAndDateDetails \ "amountCrystallised"

  val uaPathToAmendedVersion:                                     JsPath = __ \ "amendedVersion"
  val uaPathToAmountPaid:                                         JsPath = __ \ "amountPaid"
  val uaPathToAmountPaidFromPaymentDetails:                       JsPath = __ \ "paymentDetails" \ "amountPaid"

  private val uaPathToBenefitType:                                JsPath = __ \ "benefitType"
  val uaPathToFreeText:                                           JsPath = uaPathToBenefitType \ "freeText"

  private val uaPathToBeneficiaryMemberDetails:                   JsPath = __ \ "beneficiaryDetails"
  val uaPathToBeneficiaryFirstName:                               JsPath = uaPathToBeneficiaryMemberDetails \ "firstName"
  val uaPathToBeneficiaryLastName:                                JsPath = uaPathToBeneficiaryMemberDetails \ "lastName"
  val uaPathToBeneficiaryNino:                                    JsPath = uaPathToBeneficiaryMemberDetails \ "nino"

  val uaPathToBceTypeSelection:                                   JsPath = __ \ "bceTypeSelection"
  val uaPathToChooseTaxYear:                                      JsPath = __ \ "chooseTaxYear"
  val uaPathToCrystallisedAmount:                                 JsPath = __ \ "crystallisedAmount"

  private val uaPathToCrystallisedDateEvent24:                    JsPath = __ \ "crystallisedDate"
  val uaPathToTaxYearEndingDateEvent24:                           JsPath = uaPathToCrystallisedDateEvent24 \ "date"

  val uaPathToDatePaid:                                           JsPath = __ \ "datePaid"

  private val uaPathToDeceasedMemberDetails:                      JsPath = __ \ "deceasedMembersDetails"
  val uaPathToDeceasedMemberFirstName:                            JsPath = uaPathToDeceasedMemberDetails \ "firstName"
  val uaPathToDeceasedMemberLastName:                             JsPath = uaPathToDeceasedMemberDetails \ "lastName"
  val uaPathToDeceasedMemberNino:                                 JsPath = uaPathToDeceasedMemberDetails \ "nino"

  val uaPathToEmployerPayeReference:                              JsPath = __ \ "employerPayeReference"
  val uaPathToInputProtectionType:                                JsPath = __ \ "inputProtectionType"
  val uaPathToLumpSumAmount:                                      JsPath = __ \ "lumpSumAmount"

  val uaPathToEventDate:                                          JsPath = __ \ "paymentDetails" \ "eventDate"

  private val uaPathToLumpSumAmountAndDateDetails:                JsPath = __ \ "lumpSumAmountAndDate"
  val uaPathToLumpSumAmountFromDetails:                           JsPath = uaPathToLumpSumAmountAndDateDetails \ "lumpSumAmount"
  val uaPathToLumpSumDateFromDetails:                             JsPath = uaPathToLumpSumAmountAndDateDetails \ "lumpSumDate"

  private val uaPathToMembersDetails:                             JsPath = __ \ "membersDetails"
  val uaPathToMembersFirstName:                                   JsPath = uaPathToMembersDetails \ "firstName"
  val uaPathToMembersLastName:                                    JsPath = uaPathToMembersDetails \ "lastName"
  val uaPathToMembersNino:                                        JsPath = uaPathToMembersDetails \ "nino"

  val uaPathToMarginalRate:                                       JsPath = __ \ "marginalRate"
  val uaPathToMemberStatus:                                       JsPath = __ \ "memberStatus"
  val uaPathToOverAllowanceAndDeathBenefit:                       JsPath = __ \ "overAllowanceAndDeathBenefit"
  val uaPathToPaymentDate:                                        JsPath = __ \ "paymentDate" \ "date"
  val uaPathToPaymentType:                                        JsPath = __ \ "paymentType"
  val uaPathToTotalAmountBenefitCrystallisation:                  JsPath = __ \ "totalAmountBenefitCrystallisation"
  val uaPathToTotalPensionAmounts:                                JsPath = __ \ "totalPensionAmounts"

  val uaPathToReasonBenefitTaken:                                 JsPath = uaPathToBenefitType \ "reasonBenefitTaken"
  val uaPathToTypeOfProtection:                                   JsPath = __ \ "typeOfProtection"

  private val uaPathToTypeOfProtectionReferenceGroup1:            JsPath = __ \ "typeOfProtectionGroup1Reference"
  val uaPathToPreCommenceReference:                               JsPath = uaPathToTypeOfProtectionReferenceGroup1 \ "preCommencement"
  val uaPathToNonResidenceEnhancement:                            JsPath = uaPathToTypeOfProtectionReferenceGroup1 \ "nonResidenceEnhancement"
  val uaPathToOverseasReference:                                  JsPath = uaPathToTypeOfProtectionReferenceGroup1 \ "recognisedOverseasPSTE"
  val uaPathToPensionCreditReference:                             JsPath = uaPathToTypeOfProtectionReferenceGroup1 \ "pensionCreditsPreCRE"

  val uaPathToTypeOfProtectionGroup1:                             JsPath = __ \ "typeOfProtectionGroup1"
  val uaPathToTypeOfProtectionGroup2:                             JsPath = __ \ "typeOfProtectionGroup2"
  val uaPathToTypeOfProtectionGroup2Reference:                    JsPath = __ \ "typeOfProtectionGroup2Reference"
  val uaPathToTypeOfProtectionReference:                          JsPath = __ \ "typeOfProtectionReference"
  val uaPathToValidProtection:                                    JsPath = __ \ "validProtection"
}

