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
import models.enumeration.EventType.Event6
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer

object API1830 extends Transformer {
  private val pathIndividualMemberDetails = __ \ Symbol("individualDetails")
  private val pathPaymentDetails = __ \ Symbol("paymentDetails")
  private val pathAmountCrystallisedAndDateDetails = __ \ Symbol("AmountCrystallisedAndDate")

  private val readsTaxYearEndDate: Reads[JsString] = (__ \ Symbol("chooseTaxYear")).json.pick.flatMap {
    case JsString(str) => Reads.pure(JsString((str.toInt + 1).toString))
    case _ => fail[JsString]
  }

  private def readsIndividualMemberDetailsByEventType(eventType: EventType): Reads[JsObject] = {
    eventType match {
      case Event6 => readsIndividualMemberDetailsEvent6(Event6)
      case _ => readsIndividualMemberDetailsEvent22And23(eventType)
    }
  }

  private def readsIndividualMemberDetailsEvent22And23(eventType: EventType): Reads[JsObject] = {
    (
      (pathIndividualMemberDetails \ Symbol("firstName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("firstName")).json.pick) and
        (pathIndividualMemberDetails \ Symbol("lastName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("lastName")).json.pick) and
        (pathIndividualMemberDetails \ Symbol("nino")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("nino")).json.pick) and
        (__ \ Symbol("eventType")).json.put(JsString(s"Event${eventType}")) and
        (pathPaymentDetails \ Symbol("taxYearEndingDate")).json.copyFrom(readsTaxYearEndDate) and
        (pathPaymentDetails \ Symbol("monetaryAmount")).json.copyFrom((__ \ Symbol("totalPensionAmounts")).json.pick)).reduce
  }

  private def readsIndividualMemberDetailsEvent6(eventType: EventType): Reads[JsObject] = {
    (
      (pathIndividualMemberDetails \ Symbol("firstName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("firstName")).json.pick) and
        (pathIndividualMemberDetails \ Symbol("lastName")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("lastName")).json.pick) and
        (pathIndividualMemberDetails \ Symbol("nino")).json.copyFrom((__ \ Symbol("membersDetails") \ Symbol("nino")).json.pick) and
        (__ \ Symbol("eventType")).json.put(JsString(s"Event${eventType}")) and
        (pathPaymentDetails \ Symbol("amountCrystalised")).json.copyFrom((pathAmountCrystallisedAndDateDetails \ Symbol("amountCrystallised")).json.pick) and
        (pathPaymentDetails \ Symbol("typeOfProtection")).json.copyFrom((__ \ Symbol("typeOfProtection")).json.pick) and
        (pathPaymentDetails \ Symbol("eventDate")).json.copyFrom((pathAmountCrystallisedAndDateDetails \ Symbol("crystallisedDate")).json.pick) and
        (pathPaymentDetails \ Symbol("freeText")).json.copyFrom((__ \ Symbol("inputProtectionType")).json.pick)).reduce
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

