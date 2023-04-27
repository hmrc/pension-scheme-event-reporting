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

import models.enumeration.EventType
import models.enumeration.EventType.{Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer


object MemberEventReport {

  import transformations.ETMPToFrontEnd.Paths._
  import transformations.ETMPToFrontEnd.ReadsUtilities._

  def rds1832Api(eventType: EventType): Reads[JsObject] =
    pathUaEventDetailsForEventType(eventType).json.copyFrom(pathEtmpEventDetails.read(readsMembers(eventType)))

  private def readsMembers(eventType: EventType): Reads[JsArray] = __.read(Reads.seq(readsMemberDetailsByEventType(eventType))).map(JsArray(_))

  private def readsMemberDetailsByEventType(eventType: EventType): Reads[JsObject] = eventType match {
    case Event2 =>          rdsMemberDetailsEvent2
    case Event3 =>          rdsMemberDetailsEvent3
    case Event4 =>          rdsMemberDetailsEvent4
    case Event5 =>          rdsMemberDetailsEvent5
    case Event6 =>          rdsMemberDetailsEvent6
    case Event7 =>          rdsMemberDetailsEvent7
    case Event8 =>          rdsMemberDetailsEvent8
    case Event8A =>         rdsMemberDetailsEvent8A
    case _ =>               rdsMemberDetailsEvent22And23
  }

  private val rdsMemberDetailsEvent2:        Reads[JsObject] = (readsDeceasedMemberDetails and readsBeneficiaryDetails and readsEvent2PaymentDetails).reduce
  private val rdsMemberDetailsEvent3:        Reads[JsObject] = (readsMemberDetails and readsEvent3PaymentDetails).reduce
  private val rdsMemberDetailsEvent4:        Reads[JsObject] = (readsMemberDetails and readsEvent4PaymentDetails).reduce
  private val rdsMemberDetailsEvent5:        Reads[JsObject] = (readsMemberDetails and readsEvent5PaymentDetails).reduce
  private val rdsMemberDetailsEvent6:        Reads[JsObject] = (readsMemberDetails and readsEvent6PaymentDetails).reduce
  private val rdsMemberDetailsEvent7:        Reads[JsObject] = (readsMemberDetails and readsEvent7PaymentDetails).reduce
  private val rdsMemberDetailsEvent8:        Reads[JsObject] = (readsMemberDetails and readsEvent8PaymentDetails).reduce
  private val rdsMemberDetailsEvent8A:       Reads[JsObject] = (readsMemberDetails and readsEvent8APaymentDetails).reduce
  private val rdsMemberDetailsEvent22And23:  Reads[JsObject] = (readsMemberDetails and readsEvent22Or23PaymentDetails).reduce
}

private object ReadsUtilities extends Transformer {

  import transformations.ETMPToFrontEnd.Paths._

  lazy val readsEvent2PaymentDetails: Reads[JsObject] = {(
    pathUaAmountPaid.json.copyFrom(pathEtmpAmountPaid.json.pick) and
      pathUaDatePaid.json.copyFrom(pathEtmpEventDate.json.pick)
    ).reduce
  }

  lazy val readsEvent3PaymentDetails: Reads[JsObject] = {(
    pathUaReasonBenefitTaken.json.copyFrom(readsReasonBenefitTakenEvent3) and
      pathUaFreeText.json.copyFrom(pathEtmpFreeText.json.pick) and
      pathUaEventDate.json.copyFrom(pathEtmpEventDate.json.pick) and
      pathUaAmountPaidNested.json.copyFrom(pathEtmpAmountBenefit.json.pick)
    ).reduce
  }

  lazy val readsEvent4PaymentDetails: Reads[JsObject] = {(
    pathUaAmountPaidNested.json.copyFrom(pathEtmpAmountPaid.json.pick) and
      pathUaDatePaidNested.json.copyFrom(pathEtmpEventDate.json.pick)
    ).reduce
  }

  lazy val readsEvent5PaymentDetails: Reads[JsObject] = {(
      pathUaAmountPaidNested.json.copyFrom(pathEtmpAnnualRate.json.pick) and
        pathUaDatePaidNested.json.copyFrom(pathEtmpEventDate.json.pick)
      ).reduce
  }

  lazy val readsEvent6PaymentDetails: Reads[JsObject] = {(
    pathUaTypeOfProtection.json.copyFrom(readsTypeOfProtectionEvent6) and
      pathUaInputProtectionType.json.copyFrom(pathEtmpFreeText.json.pick) and
      pathUaAmountCrystallised.json.copyFrom(pathEtmpAmountCrystalised.json.pick) and
      pathUaCrystallisedDate.json.copyFrom(pathEtmpEventDate.json.pick)
    ).reduce
  }

  lazy val readsEvent7PaymentDetails: Reads[JsObject] = {(
    pathUaLumpSumAmount.json.copyFrom(pathEtmpAmountLumpSum.json.pick) and
      pathUaCrystallisedAmount.json.copyFrom(pathEtmpAmountCrystalised.json.pick) and
      pathUaPaymentDate.json.copyFrom(pathEtmpEventDate.json.pick)
    ).reduce
  }

  lazy val readsEvent8PaymentDetails: Reads[JsObject] = {(
    pathUaTypeOfProtection.json.copyFrom(readsTypeOfProtectionEvent8) and
      pathUaTypeOfProtectionReference.json.copyFrom(pathEtmpFreeText.json.pick) and
      pathUaLumpSumAmountNested.json.copyFrom(pathEtmpAmountLumpSum.json.pick) and
      pathUaLumpSumDateNested.json.copyFrom(pathEtmpEventDate.json.pick)
    ).reduce
  }

  lazy val readsEvent8APaymentDetails: Reads[JsObject] = {(
    pathUaPaymentType.json.copyFrom(readsPaymentTypeEvent8A) and
      pathUaTypeOfProtection.json.copyFrom(readsTypeOfProtectionEvent8A) and
      pathUaTypeOfProtectionReference.json.copyFrom(pathEtmpFreeText.json.pick) and
      pathUaLumpSumAmountNested.json.copyFrom(pathEtmpAmountLumpSum.json.pick) and
      pathUaLumpSumDateNested.json.copyFrom(pathEtmpEventDate.json.pick)
    ).reduce
  }

  lazy val readsEvent22Or23PaymentDetails: Reads[JsObject] = {(
    pathUaChooseTaxYearEvent.json.copyFrom(readsTaxYearEndDateEvent22And23) and
      pathUaTotalPensionAmounts.json.copyFrom(pathEtmpMonetaryAmount.json.pick)
    ).reduce
  }

  lazy val readsMemberDetails: Reads[JsObject] = {
    (
      (pathUaMembersDetails \ Symbol("firstName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("firstName")).json.pick) and
        (pathUaMembersDetails \ Symbol("lastName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("lastName")).json.pick) and
        (pathUaMembersDetails \ Symbol("nino")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("nino")).json.pick)
      ).reduce
  }

  lazy val readsDeceasedMemberDetails: Reads[JsObject] = {
    (
      (pathUaDeceasedMembersDetails \ Symbol("firstName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("firstName")).json.pick) and
        (pathUaDeceasedMembersDetails \ Symbol("lastName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("lastName")).json.pick) and
        (pathUaDeceasedMembersDetails \ Symbol("nino")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("nino")).json.pick)
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
      case _ => fail[JsString]
    }
  }

  private lazy val readsPaymentTypeEvent8A: Reads[JsString] = {
    pathEtmpReasonBenefitTaken.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(paymentTypeUAEvent8A(str)))
      case _ => fail[JsString]
    }
  }

  private lazy val readsTypeOfProtectionEvent8: Reads[JsString] = {
    pathEtmpTypeOfProtection.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(typeOfProtectionUAEvent8(str)))
      case _ => fail[JsString]
    }
  }

  private lazy val readsTypeOfProtectionEvent8A: Reads[JsString] = {
    pathEtmpTypeOfProtection.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(typeOfProtectionUAEvent8A(str)))
      case _ => fail[JsString]
    }
  }

  private lazy val readsReasonBenefitTakenEvent3: Reads[JsString] = {
    pathEtmpReasonBenefitTaken.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(reasonBenefitTakenUAEvent3(str)))
      case _ => fail[JsString]
    }
  }

  private lazy val readsTaxYearEndDateEvent22And23: Reads[JsString] = {
    pathEtmpTaxYearEndingDate.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString((str.substring(0, 4).toInt - 1).toString))
      case _ => fail[JsString]
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
    case "Enhanced" => "enhancedProtection"
  }
  private def reasonBenefitTakenUAEvent3(rBT: String): String = rBT match {
    case "Ill Health" => "illHealth"
    case "Protected Pension Age" => "protectedPensionAge"
    case "Other" => "other"
  }
}

//noinspection ScalaStyle
private object Paths {

  /* UserAnswers paths in alphabetical order */
  def pathUaEventDetailsForEventType(eventType: EventType): JsPath = __ \ Symbol(s"event${eventType.toString}") \ Symbol("members")

  // UA - nested once
  val pathUaAmountPaid:                 JsPath = __ \ Symbol("amountPaid")
  val pathUaBeneficiaryDetails:         JsPath = __ \ Symbol("beneficiaryDetails")
  val pathUaChooseTaxYearEvent:         JsPath = __ \ Symbol("chooseTaxYear")
  val pathUaCrystallisedAmount:         JsPath = __ \ Symbol("crystallisedAmount")
  val pathUaDatePaid:                   JsPath = __ \ Symbol("datePaid")
  val pathUaDeceasedMembersDetails:     JsPath = __ \ Symbol("deceasedMembersDetails")
  val pathUaInputProtectionType:        JsPath = __ \ Symbol("inputProtectionType")
  val pathUaLumpSumAmount:              JsPath = __ \ Symbol("lumpSumAmount")
  val pathUaMembersDetails:             JsPath = __ \ Symbol("membersDetails")
  val pathUaPaymentType:                JsPath = __ \ Symbol("paymentType")
  val pathUaTotalPensionAmounts:        JsPath = __ \ Symbol("totalPensionAmounts")
  val pathUaTypeOfProtection:           JsPath = __ \ Symbol("typeOfProtection")
  val pathUaTypeOfProtectionReference:  JsPath = __ \ Symbol("typeOfProtectionReference")

  // UA - nested twice
  val pathUaAmountCrystallised:         JsPath = __ \ Symbol("AmountCrystallisedAndDate") \ Symbol("amountCrystallised")
  val pathUaCrystallisedDate:           JsPath = __ \ Symbol("AmountCrystallisedAndDate") \ Symbol("crystallisedDate")
  val pathUaFreeText:                   JsPath = __ \ Symbol("benefitType") \ Symbol("freeText")
  val pathUaReasonBenefitTaken:         JsPath = __ \ Symbol("benefitType") \ Symbol("reasonBenefitTaken")
  val pathUaLumpSumAmountNested:        JsPath = __ \ Symbol("lumpSumAmountAndDate") \ Symbol("lumpSumAmount")
  val pathUaLumpSumDateNested:          JsPath = __ \ Symbol("lumpSumAmountAndDate") \ Symbol("lumpSumDate")
  val pathUaPaymentDate:                JsPath = __ \ Symbol("paymentDate") \ Symbol("date")
  val pathUaAmountPaidNested:           JsPath = __ \ Symbol("paymentDetails") \ Symbol("amountPaid")
  val pathUaDatePaidNested:             JsPath = __ \ Symbol("paymentDetails") \ Symbol("datePaid")
  val pathUaEventDate:                  JsPath = __ \ Symbol("paymentDetails") \ Symbol("eventDate")

  /* ETMP paths in alphabetical order */

  // ETMP - nested once or utils
  val pathEtmpEventDetails: JsPath = __ \ Symbol("eventDetails")
  private val pathEtmpMemberDetailEvent: JsPath = __ \ Symbol("memberDetail") \ Symbol("event")
  private val pathEtmpMemberDetailEventPaymentDetails: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails")

  // ETMP - nested from "memberDetail" \ "event"
  val pathEtmpIndividualDetails:        JsPath = pathEtmpMemberDetailEvent \ Symbol("individualDetails")
  val pathEtmpPersonReceivedThePayment: JsPath = pathEtmpMemberDetailEvent \ Symbol("personReceivedThePayment")

  // ETMP - nested from "memberDetail" \ "event" \ "paymentDetails"
  val pathEtmpAmountBenefit:            JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("amountBenefit")
  val pathEtmpAmountCrystalised:        JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("amountCrystalised")
  val pathEtmpAmountLumpSum:            JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("amountLumpSum")
  val pathEtmpAmountPaid:               JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("amountPaid")
  val pathEtmpAnnualRate:               JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("annualRate")
  val pathEtmpEventDate:                JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("eventDate")
  val pathEtmpFreeText:                 JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("freeText")
  val pathEtmpMonetaryAmount:           JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("monetaryAmount")
  val pathEtmpReasonBenefitTaken:       JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("reasonBenefitTaken")
  val pathEtmpTaxYearEndingDate:        JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("taxYearEndingDate")
  val pathEtmpTypeOfProtection:         JsPath = pathEtmpMemberDetailEventPaymentDetails \ Symbol("typeOfProtection")
}
