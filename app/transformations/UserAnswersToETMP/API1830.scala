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

import models.enumeration.EventType
import models.enumeration.EventType.{Event2, Event6, Event7, Event8, Event8A}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer

object API1830 extends Transformer {
  private val pathIndividualMemberDetails = __ \ Symbol("individualDetails")
  private val pathPaymentDetails = __ \ Symbol("paymentDetails")
  private val pathAmountCrystallisedAndDateDetails = __ \ Symbol("AmountCrystallisedAndDate")
  private val pathLumpSumAmountAndDateDetails = __ \ Symbol("lumpSumAmountAndDate")
  private val pathCrystallisedAmount = __ \ Symbol("crystallisedAmount")

  private val pathPersonReceivedThePayment = __ \ Symbol("personReceivedThePayment")
  private val pathDeceasedMemberDetails = __ \ Symbol("deceasedMembersDetails")
  private val pathBeneficiaryMemberDetails = __ \ Symbol("beneficiaryDetails")

  private val readsTaxYearEndDate: Reads[JsString] = (__ \ Symbol("chooseTaxYear")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString((str.toInt + 1).toString))
    case _ => fail[JsString]
  }

  private val readsTypeOfProtectionEvent6: Reads[JsString] = (__ \ Symbol("typeOfProtection")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event6TypeOfProtectionConversion(str)))
    case _ => fail[JsString]
  }

  private val readsTypeOfProtectionEvent8: Reads[JsString] = (__ \ Symbol("typeOfProtection")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event8TypeOfProtectionConversion(str)))
    case _ => fail[JsString]
  }

  private val readsTypeOfProtectionEvent8A: Reads[JsString] = (__ \ Symbol("typeOfProtection")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event8ATypeOfProtectionConversion(str)))
    case _ => fail[JsString]
  }

  private val readsPaymentTypeEvent8A: Reads[JsString] = (__ \ Symbol("paymentType")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(event8APaymentTypeConversion(str)))
    case _ => fail[JsString]
  }

  private val readsTypeOfProtectionReferenceEvent8A: Reads[JsString] = (__ \ Symbol("typeOfProtectionReference")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(str))
    case _ => fail[JsString]
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
    case "enhancedProtection" => "Enhanced"
  }

  //noinspection ScalaStyle
  private def event8APaymentTypeConversion(pT: String): String = pT match {
    case "paymentOfAStandAloneLumpSum" =>
      "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection"
    case "paymentOfASchemeSpecificLumpSum" =>
      "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance"
  }

  private def readsIndividualMemberDetailsByEventType(eventType: EventType): Reads[JsObject] = {
    eventType match {
      case Event2 => readsDeceasedAndBeneficiaryMemberDetailsEvent2
      case Event6 => readsIndividualMemberDetailsEvent6
      case Event7 => readsIndividualMemberDetailsEvent7
      case Event8 => readsIndividualMemberDetailsEvent8
      case Event8A => readsIndividualMemberDetailsEvent8A
      case _ => readsIndividualMemberDetailsEvent22And23(eventType)
    }
  }

  private val readsIndividualMemberDetails: Reads[JsObject] = {
    ((pathIndividualMemberDetails \ Symbol("firstName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("firstName")).json.pick) and
      (pathIndividualMemberDetails \ Symbol("lastName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("lastName")).json.pick) and
      (pathIndividualMemberDetails \ Symbol("nino")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("nino")).json.pick)).reduce
  }
  /*
   */

  private def readsDeceasedAndBeneficiaryMemberDetailsEvent2: Reads[JsObject] = {
    (
      (__ \ Symbol("eventType")).json.put(JsString(s"Event$Event2")) and
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

  private def readsIndividualMemberDetailsEvent22And23(eventType: EventType): Reads[JsObject] = {
    (
      readsIndividualMemberDetails and
        (__ \ Symbol("eventType")).json.put(JsString(s"Event${eventType}")) and
        (pathPaymentDetails \ Symbol("taxYearEndingDate")).json.copyFrom(readsTaxYearEndDate) and
        (pathPaymentDetails \ Symbol("monetaryAmount")).json.copyFrom((__ \ Symbol("totalPensionAmounts")).json.pick)).reduce
  }

  private def readsIndividualMemberDetailsEvent6: Reads[JsObject] = {
    (
      readsIndividualMemberDetails and
        (__ \ Symbol("eventType")).json.put(JsString(s"Event$Event6")) and
        (pathPaymentDetails \ Symbol("amountCrystalised")).json.copyFrom((pathAmountCrystallisedAndDateDetails \ Symbol("amountCrystallised")).json.pick) and
        (pathPaymentDetails \ Symbol("typeOfProtection")).json.copyFrom(readsTypeOfProtectionEvent6) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((pathAmountCrystallisedAndDateDetails \ Symbol("crystallisedDate")).json.pick) and
        (pathPaymentDetails \ Symbol("freeText")).json.copyFrom((__ \ Symbol("inputProtectionType")).json.pick)).reduce
  }

  private def readsIndividualMemberDetailsEvent7: Reads[JsObject] = {
    (
      readsIndividualMemberDetails and
        (__ \ Symbol("eventType")).json.put(JsString(s"Event$Event7")) and
        (pathPaymentDetails \ Symbol("amountLumpSum")).json.copyFrom((__ \ Symbol("lumpSumAmount")).json.pick) and
        (pathPaymentDetails \ Symbol("amountCrystalised")).json.copyFrom((__ \ Symbol("crystallisedAmount")).json.pick) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((__ \ Symbol("paymentDate") \ Symbol("date")).json.pick)).reduce
  }

  private def readsIndividualMemberDetailsEvent8: Reads[JsObject] = {
    (
      readsIndividualMemberDetails and
        (__ \ Symbol("eventType")).json.put(JsString(s"Event$Event8")) and
        (pathPaymentDetails \ Symbol("amountLumpSum")).json.copyFrom((pathLumpSumAmountAndDateDetails \ Symbol("lumpSumAmount")).json.pick) and
        (pathPaymentDetails \ Symbol("typeOfProtection")).json.copyFrom(readsTypeOfProtectionEvent8) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((pathLumpSumAmountAndDateDetails \ Symbol("lumpSumDate")).json.pick) and
        (pathPaymentDetails \ Symbol("freeText")).json.copyFrom((__ \ Symbol("typeOfProtectionReference")).json.pick)).reduce
  }

  private def readsIndividualMemberDetailsEvent8A: Reads[JsObject] = {
    (
      readsIndividualMemberDetails and
        (__ \ Symbol("eventType")).json.put(JsString(s"Event$Event8A")) and
        (pathPaymentDetails \ Symbol("reasonBenefitTaken")).json.copyFrom(readsPaymentTypeEvent8A) and
        (pathPaymentDetails \ Symbol("typeOfProtection")).json.copyFrom(readsTypeOfProtectionEvent8A.orElse((Reads.pure(JsString("N/A"))))) and
        (pathPaymentDetails \ Symbol("amountLumpSum")).json.copyFrom((pathLumpSumAmountAndDateDetails \ Symbol("lumpSumAmount")).json.pick) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((pathLumpSumAmountAndDateDetails \ Symbol("lumpSumDate")).json.pick) and
        (pathPaymentDetails \ Symbol("freeText")).json.copyFrom(readsTypeOfProtectionReferenceEvent8A.orElse(Reads.pure(JsString("N/A"))))).reduce
  }

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
        val jsonArray = optionJsArray.getOrElse(Json.arr())
        Json.obj("memberEventsDetails" -> (Json.obj(
          "eventDetails" -> jsonArray
        ) ++ fullHdr))
      }
    }
  }
}

