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
import transformations.ETMPToFrontEnd.API1832Paths._
import transformations.Transformer


object API1832 {

  import transformations.ETMPToFrontEnd.API1832ReadsUtilities._
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
  def pathUaEventDetailsForEventType(eventType: EventType): JsPath = __ \ Symbol(s"event${eventType.toString}") \ Symbol("members")

  lazy val readsEvent2PaymentDetails: Reads[JsObject] = (
      reqReads(pathUaAmountPaid, pathEtmpAmountPaid) and
      reqReads(pathUaDatePaid, pathEtmpEventDate)
      ).reduce

  lazy val readsEvent3PaymentDetails: Reads[JsObject] = (
      reqNestedReadsJsString(pathUaReasonBenefitTaken, readsReasonBenefitTakenEvent3) and
      optReads(pathUaFreeText, pathEtmpFreeText) and
      reqReads(pathUaEventDate, pathEtmpEventDate) and
      reqReads(pathUaAmountPaidNested, pathEtmpAmountBenefit)
      ).reduce

  lazy val readsEvent4PaymentDetails: Reads[JsObject] = (
      reqReads(pathUaAmountPaidNested, pathEtmpAmountPaid) and
      reqReads(pathUaEventDate, pathEtmpEventDate)
      ).reduce

  lazy val readsEvent5PaymentDetails: Reads[JsObject] = (
      reqReads(pathUaAmountPaidNested, pathEtmpAnnualRate) and
      reqReads(pathUaEventDate, pathEtmpEventDate)
      ).reduce

  lazy val readsEvent6PaymentDetails: Reads[JsObject] = (
      reqNestedReadsJsString(pathUaTypeOfProtection, readsTypeOfProtectionEvent6) and
      reqReads(pathUaInputProtectionType, pathEtmpFreeText) and
      reqReads(pathUaAmountCrystallised, pathEtmpAmountCrystalised) and
      reqReads(pathUaCrystallisedDate, pathEtmpEventDate)
      ).reduce

  lazy val readsEvent7PaymentDetails: Reads[JsObject] = (
      reqReads(pathUaLumpSumAmount, pathEtmpAmountLumpSum) and
      reqReads(pathUaCrystallisedAmount, pathEtmpAmountCrystalised) and
      reqReads(pathUaPaymentDate, pathEtmpEventDate)
      ).reduce

  lazy val readsEvent8PaymentDetails: Reads[JsObject] = (
      reqNestedReadsJsString(pathUaTypeOfProtection, readsTypeOfProtectionEvent8) and
      reqReads(pathUaTypeOfProtectionReference, pathEtmpFreeText) and
      reqReads(pathUaLumpSumAmountNested, pathEtmpAmountLumpSum) and
      reqReads(pathUaLumpSumDateNested, pathEtmpEventDate)
      ).reduce

  lazy val readsEvent24PaymentDetails: Reads[JsObject] = (
    reqReadsBoolTransform(pathUaMemberHoldProtection, pathEtmpMemberHoldProtectionEvent24, yesNoTransformToBoolean) and
      optNestedReadsJsString(pathUaTypeOfProtectionEvent24, readsTypeOfProtectionEvent24) and
      optNestedReadsJsArray(pathUaProtectionGroup1Event24, readsTypeOfProtectionGroup1Event24) and
      optNestedReadsJsString(pathUaPreCommenceReference, readsPreCommenceReference) and
      optNestedReadsJsString(pathUaPensionCreditReference, readsPensionCreditReference) and
      optNestedReadsJsString(pathUaNonResidenceReference, readsNonResidenceReference) and
      optNestedReadsJsString(pathUaOverseasReference, readsOverseasReference) and
      optReadsBoolTransform(pathUaAvailableLumpSumDBAExceeded, pathEtmpAvailableLumpSumDBAExceeded, yesNoTransformToBoolean) and
      optReadsBoolTransform(pathUaAvailableLumpSumExceeded, pathEtmpAvailableLumpSumExceeded, yesNoTransformToBoolean) and
      reqReads(pathUaAmountCrystalised, pathEtmpAmountCrystalised) and
      reqNestedReadsJsString(pathUaBCEType, readsBCETypeEvent24) and
      reqReads(pathUaCrystallisedDateEvent24, pathEtmpTaxYearEndingDate) and
      optReads(pathUaFreeTextEvent24, pathEtmpFreeText) and
      optReadsBoolTransform(pathUaMarginalRate, pathEtmpMarginalRate, yesNoTransformToBoolean) and
      optReads(pathUaPayeReference, pathEtmpPayeReference)
      ).reduce

  private def readAdditiveIfPresent(etmpPath: JsPath, uaPath: JsPath, f: String => String = identity): JsObject => Reads[JsObject] = jsObj =>
    etmpPath.readNullable[String].flatMap {
      case None => Reads.pure(jsObj)
      case Some(str) => uaPath.json.put(JsString(f(str))).map(_ ++ jsObj)
    }

  lazy val readsEvent8APaymentDetails: Reads[JsObject] = (
      reqReads(pathUaLumpSumAmountNested, pathEtmpAmountLumpSum) and
      reqReads(pathUaLumpSumDateNested, pathEtmpEventDate)
      ).reduce
      .flatMap(readAdditiveIfPresent(pathEtmpTypeOfProtection, pathUaTypeOfProtection, typeOfProtectionUAEvent8A))
      .flatMap(readAdditiveIfPresent(pathEtmpReasonBenefitTaken, pathUaPaymentType, paymentTypeUAEvent8A))
      .flatMap(readAdditiveIfPresent(pathEtmpFreeText, pathUaTypeOfProtectionReference))

  lazy val readsEvent22Or23PaymentDetails: Reads[JsObject] = (
      reqNestedReadsJsString(pathUaChooseTaxYearEvent, readsTaxYearEndDateEvent22And23) and
      reqReads(pathUaTotalPensionAmounts, pathEtmpMonetaryAmount)
      ).reduce

  lazy val readsMemberDetails: Reads[JsObject] = (
    reqReads(pathUaMemberFirstName, pathEtmpIndividualFirstName) and
    reqReads(pathUaMemberLastName, pathEtmpIndividualLastName) and
    reqReads(pathUaMemberNino, pathEtmpIndividualNino) and
    optReads(pathUaMemberStatus, pathEtmlMemberStatus) and
    optReads(pathUaAmendedVersion, pathEtmlAmendedVersion)
    ).reduce

  lazy val readsDeceasedMemberDetails: Reads[JsObject] = (
    reqReads(pathUaDeceasedMemberFirstName, pathEtmpIndividualFirstName) and
    reqReads(pathUaDeceasedMemberLastName, pathEtmpIndividualLastName) and
    reqReads(pathUaDeceasedMemberNino, pathEtmpIndividualNino) and
    optReads(pathUaMemberStatus, pathEtmlMemberStatus) and
    optReads(pathUaAmendedVersion, pathEtmlAmendedVersion)
    ).reduce

  lazy val readsBeneficiaryDetails: Reads[JsObject] = (
    reqReads(pathUaBeneficiaryFirstName, pathEtmpReceiverFirstName) and
    reqReads(pathUaBeneficiaryLastName, pathEtmpReceiverLastName) and
    reqReads(pathUaBeneficiaryNino, pathEtmpReceiverNino)
    ).reduce

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

  private lazy val readsPreCommenceReference: Reads[JsString] = {
    pathEtmpPreCommenceReference.readNullable[String].map {
      case Some(str) => JsString(str)
      case _ => JsString("")
    }
  }

  private lazy val readsPensionCreditReference: Reads[JsString] = {
    pathEtmpPensionCreditReference.readNullable[String].map {
      case Some(str) => JsString(str)
      case _ => JsString("")
    }
  }

  private lazy val readsNonResidenceReference: Reads[JsString] = {
    pathEtmpNonResidenceReference.readNullable[String].map {
      case Some(str) => JsString(str)
      case _ => JsString("")
    }
  }

  private lazy val readsOverseasReference: Reads[JsString] = {
    pathEtmpOverseasReference.readNullable[String].map {
      case Some(str) => JsString(str)
      case _ => JsString("")
    }
  }

  private lazy val readsTypeOfProtectionGroup1Event24: Reads[JsArray] = {
    for {
      preCommencement <- pathEtmpPreCommenceReference.readNullable[String].map {
        case Some(_) => "preCommencement"
        case None => ""
      }
      pensionCreditsPreCRE <- pathEtmpPensionCreditReference.readNullable[String].map {
        case Some(_) => "pensionCreditsPreCRE"
        case None => ""
      }
      nonResidenceEnhancement <- pathEtmpNonResidenceReference.readNullable[String].map {
        case Some(_) => "nonResidenceEnhancement"
        case None => ""
      }
      recognisedOverseasPSTE <- pathEtmpOverseasReference.readNullable[String].map {
        case Some(_) => "recognisedOverseasPSTE"
        case None => ""
      }
      schemeSpecific <- pathEtmpSchemeSpecificLumpSum.readNullable[String].map {
        case Some(str) if str == "Yes" => "schemeSpecific"
        case _ => ""
      }
    } yield {
      val filteredStrings = Seq(
        JsString(preCommencement),
        JsString(pensionCreditsPreCRE),
        JsString(nonResidenceEnhancement),
        JsString(recognisedOverseasPSTE),
        JsString(schemeSpecific)
      ).filter(_ != JsString(""))

      JsArray(filteredStrings)
    }
  }

  private lazy val readsBCETypeEvent24: Reads[JsString] = {
    pathEtmpBCEType.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(bceTypeUAEvent24(str)))
      case _ => fail(JsString("bceTypeEvent24"))
    }
  }

  lazy val reqReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick)
  private lazy val reqNestedReadsJsString: (JsPath, Reads[JsString]) => Reads[JsObject] =
    (uaPath: JsPath, etmpReads: Reads[JsString]) => uaPath.json.copyFrom(etmpReads)

  private lazy val reqReadsBoolTransform: (JsPath, JsPath, String => Boolean) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => Boolean) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsBoolean(transform(str)))
        case _ => fail[JsBoolean]
      })
    }

  lazy val optReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick).orElse(doNothing)

  private lazy val optNestedReadsJsString: (JsPath, Reads[JsString]) => Reads[JsObject] =
    (uaPath: JsPath, etmpReads: Reads[JsString]) => uaPath.json.copyFrom(etmpReads).orElse(doNothing)

  private lazy val optNestedReadsJsArray: (JsPath, Reads[JsArray]) => Reads[JsObject] =
    (uaPath: JsPath, etmpReads: Reads[JsArray]) => uaPath.json.copyFrom(etmpReads).orElse(doNothing)

  private lazy val optReadsBoolTransform: (JsPath, JsPath, String => Boolean) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => Boolean) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsBoolean(transform(str)))
        case _ => fail[JsBoolean]
      }).orElse(doNothing)
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
    case "Primary Protection" => "primary"
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

private object API1832Paths {
  // UA

  // UA - nested once
  val pathUaAmountPaid:                                 JsPath = __ \ "amountPaid"
  val pathUaMemberStatus:                               JsPath = __ \ "memberStatus"
  val pathUaAmendedVersion:                             JsPath = __ \ "amendedVersion"
  val pathUaAmountCrystalised:                          JsPath = __ \ "totalAmountBenefitCrystallisation"
  val pathUaAvailableLumpSumExceeded:                   JsPath = __ \ "overAllowance"
  val pathUaAvailableLumpSumDBAExceeded:                JsPath = __ \ "overAllowanceAndDeathBenefit"
  val pathUaBCEType:                                    JsPath = __ \ "bceTypeSelection"
  val pathUaBeneficiaryDetails:                         JsPath = __ \ "beneficiaryDetails"
  val pathUaChooseTaxYearEvent:                         JsPath = __ \ "chooseTaxYear"
  val pathUaCrystallisedAmount:                         JsPath = __ \ "crystallisedAmount"
  val pathUaDatePaid:                                   JsPath = __ \ "datePaid"
  val pathUaDeceasedMembersDetails:                     JsPath = __ \ "deceasedMembersDetails"
  val pathUaFreeTextEvent24:                            JsPath = __ \ "typeOfProtectionGroup2Reference"
  val pathUaInputProtectionType:                        JsPath = __ \ "inputProtectionType"
  val pathUaLumpSumAmount:                              JsPath = __ \ "lumpSumAmount"
  val pathUaMarginalRate:                               JsPath = __ \ "marginalRate"
  val pathUaMemberHoldProtection:                       JsPath = __ \ "validProtection"
  val pathUaMembersDetails:                             JsPath = __ \ "membersDetails"
  val pathUaPayeReference:                              JsPath = __ \ "employerPayeReference"
  val pathUaPaymentType:                                JsPath = __ \ "paymentType"
  val pathUaProtectionGroup1Event24:                    JsPath = __ \ "typeOfProtectionGroup1"
  val pathUaProtectionRefGroup1:                        JsPath = __ \ "typeOfProtectionGroup1Reference"
  val pathUaTotalPensionAmounts:                        JsPath = __ \ "totalPensionAmounts"
  val pathUaTypeOfProtection:                           JsPath = __ \ "typeOfProtection"
  val pathUaTypeOfProtectionEvent24:                    JsPath = __ \ "typeOfProtectionGroup2"
  val pathUaTypeOfProtectionReference:                  JsPath = __ \ "typeOfProtectionReference"

  // UA - nested twice
  val pathUaAmountCrystallised:                         JsPath = __ \ "AmountCrystallisedAndDate" \ "amountCrystallised"
  val pathUaAmountPaidNested:                           JsPath = __ \ "paymentDetails" \ "amountPaid"
  val pathUaBeneficiaryFirstName:                       JsPath = pathUaBeneficiaryDetails \ "firstName"
  val pathUaBeneficiaryLastName:                        JsPath = pathUaBeneficiaryDetails \ "lastName"
  val pathUaBeneficiaryNino:                            JsPath = pathUaBeneficiaryDetails \ "nino"
  val pathUaCrystallisedDate:                           JsPath = __ \ "AmountCrystallisedAndDate" \ "crystallisedDate"
  val pathUaCrystallisedDateEvent24:                    JsPath = __ \ "crystallisedDate" \ "date"
  val pathUaDeceasedMemberFirstName:                    JsPath = pathUaDeceasedMembersDetails \ "firstName"
  val pathUaDeceasedMemberLastName:                     JsPath = pathUaDeceasedMembersDetails \ "lastName"
  val pathUaDeceasedMemberNino:                         JsPath = pathUaDeceasedMembersDetails \ "nino"
  val pathUaEventDate:                                  JsPath = __ \ "paymentDetails" \ "eventDate"
  val pathUaFreeText:                                   JsPath = __ \ "benefitType" \ "freeText"
  val pathUaLumpSumAmountNested:                        JsPath = __ \ "lumpSumAmountAndDate" \ "lumpSumAmount"
  val pathUaLumpSumDateNested:                          JsPath = __ \ "lumpSumAmountAndDate" \ "lumpSumDate"
  val pathUaMemberFirstName:                            JsPath = pathUaMembersDetails \ "firstName"
  val pathUaMemberLastName:                             JsPath = pathUaMembersDetails \ "lastName"
  val pathUaMemberNino:                                 JsPath = pathUaMembersDetails \ "nino"
  val pathUaNonResidenceReference:                      JsPath = pathUaProtectionRefGroup1 \ "nonResidenceEnhancement"
  val pathUaOverseasReference:                          JsPath = pathUaProtectionRefGroup1 \ "recognisedOverseasPSTE"
  val pathUaPaymentDate:                                JsPath = __ \ "paymentDate" \ "date"
  val pathUaPensionCreditReference:                     JsPath = pathUaProtectionRefGroup1 \ "pensionCreditsPreCRE"
  val pathUaPreCommenceReference:                       JsPath = pathUaProtectionRefGroup1 \ "preCommencement"
  val pathUaReasonBenefitTaken:                         JsPath = __ \ "benefitType" \ "reasonBenefitTaken"


  // ETMP

  // ETMP - nested once or utils
  val pathEtmpEventDetails:                             JsPath = __ \ "eventDetails"

  private val pathEtmpMemberDetail:                     JsPath = __ \ "memberDetail"
  val pathEtmlAmendedVersion:                           JsPath = pathEtmpMemberDetail \ "amendedVersion"
  val pathEtmlMemberStatus:                             JsPath = pathEtmpMemberDetail \ "memberStatus"

  private val pathEtmpMemberDetailEvent:                JsPath = pathEtmpMemberDetail \ "event"
  private val pathEtmpMemberDetailEventPaymentDetails:  JsPath = pathEtmpMemberDetailEvent \ "paymentDetails"

  // ETMP - nested from "memberDetail" \ "event"
  val pathEtmpIndividualDetails:                        JsPath = pathEtmpMemberDetailEvent \ "individualDetails"
  val pathEtmpPersonReceivedThePayment:                 JsPath = pathEtmpMemberDetailEvent \ "personReceivedThePayment"

  // ETMP - nested from "memberDetail" \ "event" \ "paymentDetails"
  val pathEtmpAmountBenefit:                            JsPath = pathEtmpMemberDetailEventPaymentDetails \ "amountBenefit"
  val pathEtmpAmountCrystalised:                        JsPath = pathEtmpMemberDetailEventPaymentDetails \ "amountCrystalised"
  val pathEtmpAmountLumpSum:                            JsPath = pathEtmpMemberDetailEventPaymentDetails \ "amountLumpSum"
  val pathEtmpAmountPaid:                               JsPath = pathEtmpMemberDetailEventPaymentDetails \ "amountPaid"
  val pathEtmpAnnualRate:                               JsPath = pathEtmpMemberDetailEventPaymentDetails \ "annualRate"
  val pathEtmpAvailableLumpSumExceeded:                 JsPath = pathEtmpMemberDetailEventPaymentDetails \ "availableLumpSumExceeded"
  val pathEtmpAvailableLumpSumDBAExceeded:              JsPath = pathEtmpMemberDetailEventPaymentDetails \ "availableLumpSumDBAExceeded"
  val pathEtmpBCEType:                                  JsPath = pathEtmpMemberDetailEventPaymentDetails \ "reasonBenefitTaken"
  val pathEtmpEventDate:                                JsPath = pathEtmpMemberDetailEventPaymentDetails \ "eventDate"
  val pathEtmpFreeText:                                 JsPath = pathEtmpMemberDetailEventPaymentDetails \ "freeText"
  val pathEtmpIndividualFirstName:                      JsPath = pathEtmpIndividualDetails \ "firstName"
  val pathEtmpIndividualLastName:                       JsPath = pathEtmpIndividualDetails \ "lastName"
  val pathEtmpIndividualNino:                           JsPath = pathEtmpIndividualDetails \ "nino"
  val pathEtmpMarginalRate:                             JsPath = pathEtmpMemberDetailEventPaymentDetails \ "taxedAtMarginalRate"
  val pathEtmpMemberHoldProtectionEvent24:              JsPath = pathEtmpMemberDetailEventPaymentDetails \ "memberHoldProtection"
  val pathEtmpMonetaryAmount:                           JsPath = pathEtmpMemberDetailEventPaymentDetails \ "monetaryAmount"
  val pathEtmpNonResidenceReference:                    JsPath = pathEtmpMemberDetailEventPaymentDetails \ "nonResidenceReference"
  val pathEtmpOverseasReference:                        JsPath = pathEtmpMemberDetailEventPaymentDetails \ "overseasReference"
  val pathEtmpPayeReference:                            JsPath = pathEtmpMemberDetailEventPaymentDetails \ "payeReference"
  val pathEtmpPensionCreditReference:                   JsPath = pathEtmpMemberDetailEventPaymentDetails \ "pensionCreditReference"
  val pathEtmpPreCommenceReference:                     JsPath = pathEtmpMemberDetailEventPaymentDetails \ "preCommenceReference"
  val pathEtmpReasonBenefitTaken:                       JsPath = pathEtmpMemberDetailEventPaymentDetails \ "reasonBenefitTaken"
  val pathEtmpReceiverFirstName:                        JsPath = pathEtmpPersonReceivedThePayment \ "firstName"
  val pathEtmpReceiverLastName:                         JsPath = pathEtmpPersonReceivedThePayment \ "lastName"
  val pathEtmpReceiverNino:                             JsPath = pathEtmpPersonReceivedThePayment \ "nino"
  val pathEtmpSchemeSpecificLumpSum:                    JsPath = pathEtmpMemberDetailEventPaymentDetails \ "schemeSpecificLumpSum"
  val pathEtmpTaxYearEndingDate:                        JsPath = pathEtmpMemberDetailEventPaymentDetails \ "taxYearEndingDate"
  val pathEtmpTypeOfProtection:                         JsPath = pathEtmpMemberDetailEventPaymentDetails \ "typeOfProtection"
}