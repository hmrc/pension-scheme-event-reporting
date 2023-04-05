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
import models.enumeration.EventType.{Event6, Event8, Event8A}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer

object API1830 extends Transformer {
  private val pathIndividualMemberDetails = __ \ Symbol("individualDetails")
  private val pathPaymentDetails = __ \ Symbol("paymentDetails")
  private val pathAmountCrystallisedAndDateDetails = __ \ Symbol("AmountCrystallisedAndDate")
  private val pathLumpSumAmountAndDateDetails = __ \ Symbol("lumpSumAmountAndDate")

  private val readsTaxYearEndDate: Reads[JsString] = (__ \ Symbol("chooseTaxYear")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString(s"${str.toInt + 1}-04-05"))
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
      case Event6 => readsIndividualMemberDetailsEvent6(Event6)
      case Event8 => readsIndividualMemberDetailsEvent8(Event8)
      case Event8A => readsIndividualMemberDetailsEvent8A(Event8A)
      case _ => readsIndividualMemberDetailsEvent22And23(eventType)
    }
  }

  private val readsIndividualMemberDetails: Reads[JsObject] = {
    ((pathIndividualMemberDetails \ Symbol("firstName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("firstName")).json.pick) and
      (pathIndividualMemberDetails \ Symbol("lastName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("lastName")).json.pick) and
      (pathIndividualMemberDetails \ Symbol("nino")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("nino")).json.pick)).reduce
  }

  private def readsIndividualMemberDetailsEvent22And23(eventType: EventType): Reads[JsObject] = {
    (
      readsIndividualMemberDetails and
        (__ \ Symbol("eventType")).json.put(JsString(s"Event${eventType}")) and
        (pathPaymentDetails \ Symbol("taxYearEndingDate")).json.copyFrom(readsTaxYearEndDate) and
        (pathPaymentDetails \ Symbol("monetaryAmount")).json.copyFrom((__ \ Symbol("totalPensionAmounts")).json.pick)).reduce
  }

  private def readsIndividualMemberDetailsEvent6(eventType: EventType): Reads[JsObject] = {
    (
      readsIndividualMemberDetails and
        (__ \ Symbol("eventType")).json.put(JsString(s"Event${eventType}")) and
        (pathPaymentDetails \ Symbol("amountCrystalised")).json.copyFrom((pathAmountCrystallisedAndDateDetails \ Symbol("amountCrystallised")).json.pick) and
        (pathPaymentDetails \ Symbol("typeOfProtection")).json.copyFrom(readsTypeOfProtectionEvent6) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((pathAmountCrystallisedAndDateDetails \ Symbol("crystallisedDate")).json.pick) and
        (pathPaymentDetails \ Symbol("freeText")).json.copyFrom((__ \ Symbol("inputProtectionType")).json.pick)).reduce
  }

  private def readsIndividualMemberDetailsEvent8(eventType: EventType): Reads[JsObject] = {
    (
      readsIndividualMemberDetails and
        (__ \ Symbol("eventType")).json.put(JsString(s"Event$eventType")) and
        (pathPaymentDetails \ Symbol("amountLumpSum")).json.copyFrom((pathLumpSumAmountAndDateDetails \ Symbol("lumpSumAmount")).json.pick) and
        (pathPaymentDetails \ Symbol("typeOfProtection")).json.copyFrom(readsTypeOfProtectionEvent8) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((pathLumpSumAmountAndDateDetails \ Symbol("lumpSumDate")).json.pick) and
        (pathPaymentDetails \ Symbol("freeText")).json.copyFrom((__ \ Symbol("typeOfProtectionReference")).json.pick)).reduce
  }

  private def readsIndividualMemberDetailsEvent8A(eventType: EventType): Reads[JsObject] = {
    (
      readsIndividualMemberDetails and
        (__ \ Symbol("eventType")).json.put(JsString(s"Event$eventType")) and
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

