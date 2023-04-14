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
import transformations.Transformer
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._


//noinspection ScalaStyle
object MemberEventReport {

  import transformations.ETMPToFrontEnd.ReadsWithTransform._
  import transformations.ETMPToFrontEnd.Paths._

  def rds1832Api(eventType: EventType): Reads[JsObject] = pathUaEventDetailsForEventType(eventType).json.copyFrom(pathEtmpEventDetails.read(readsMembers(eventType)))

  private def readsMembers(eventType: EventType): Reads[JsArray] = __.read(Reads.seq(readsMemberDetailsByEventType(eventType))).map(JsArray(_))

  private def readsMemberDetailsByEventType(eventType: EventType): Reads[JsObject] = eventType match {
    case Event2 => rdsMemberDetailsEvent2
    case Event3 => ???
    case Event4 | Event5 => rdsMemberDetailsEvent4And5
    case Event6 => rdsMemberDetailsEvent6
    case Event7 => rdsMemberDetailsEvent7
    case Event8 => ???
    case Event8A => ???
    case _ => rdsMemberDetailsEvent22And23
  }

  implicit val rdsMemberDetailsEvent2: Reads[JsObject] = {(

    (pathDeceasedMemberDetails \ Symbol("firstName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("firstName")).json.pick) and
      (pathDeceasedMemberDetails \ Symbol("lastName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("lastName")).json.pick) and
      (pathDeceasedMemberDetails \ Symbol("nino")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("nino")).json.pick) and
      (pathBeneficiaryMemberDetails \ Symbol("firstName")).json.copyFrom((pathDeceasedMemberDetails \ Symbol("firstName")).json.pick) and
      (pathBeneficiaryMemberDetails \ Symbol("lastName")).json.copyFrom((pathDeceasedMemberDetails \ Symbol("lastName")).json.pick) and
      (pathBeneficiaryMemberDetails \ Symbol("nino")).json.copyFrom((pathDeceasedMemberDetails \ Symbol("nino")).json.pick) and
      (__ \ Symbol("amountPaid")).json.copyFrom((pathPaymentDetailsEvent2 \ Symbol("amountPaid")).json.pick) and
      (__ \ Symbol("datePaid")).json.copyFrom((pathPaymentDetailsEvent2 \ Symbol("eventDate")).json.pick)
    ).reduce
  }

  implicit val rdsMemberDetailsEvent4And5: Reads[JsObject] = {(
    readsMemberDetails and
        (pathPaymentDetails \ Symbol("amountPaid")).json.copyFrom((pathEtmpMonetaryAmountEvent4And5).json.pick) and
        (pathPaymentDetails \ Symbol("datePaid")).json.copyFrom((pathEtmpDateEvent4And5).json.pick)
    ).reduce
  }

  implicit val rdsMemberDetailsEvent6: Reads[JsObject] = {
    (
      readsMemberDetails and
        (pathPaymentDetails \ Symbol("amountPaid")).json.copyFrom(pathEtmpMonetaryAmountEvent4And5.json.pick) and
        (pathPaymentDetails \ Symbol("datePaid")).json.copyFrom(pathEtmpDateEvent4And5.json.pick) and
        (__ \ Symbol("typeOfProtection")).json.copyFrom(pathEtmpTypeOfProtectionEvent6.json.pick) and
        (__ \ Symbol("inputProtectionType")).json.copyFrom(pathEtmpInputProtectionType.json.pick) and
        (pathPaymentDetailsEvent6 \ Symbol("amountCrystallised")).json.copyFrom(pathEtmpAmountCrystallisedEvent6.json.pick) and
        (pathPaymentDetailsEvent6 \ Symbol("crystallisedDate")).json.copyFrom(pathEtmpEventDateEvent6.json.pick)
      ).reduce
  }

  implicit val rdsMemberDetailsEvent7: Reads[JsObject] = {
    (
      readsMemberDetails and
        (__ \ Symbol("lumpSumAmount")).json.copyFrom((pathEtmpAmountLumpSumEvent7).json.pick) and
        (__ \ Symbol("crystallisedAmount")).json.copyFrom((pathEtmpAmountCrystallisedEvent7).json.pick) and
        (__ \ Symbol("paymentDate") \ Symbol("date")).json.copyFrom((pathEtmpDateEvent7).json.pick)
      ).reduce
  }

  implicit val rdsMemberDetailsEvent22And23: Reads[JsObject] = {(
    readsMemberDetails and
      pathUaChooseTaxYearEvent22And23.json.copyFrom(readsTaxYearEndDateEvent22And23) and
     pathUaTotalPensionAmountsEvent22And23.json.copyFrom(pathEtmpMonetaryAmountEvent22And23.json.pick)
    ).reduce
  }
}

private object Paths {

  def pathUaEventDetailsForEventType(eventType: EventType): JsPath = __ \ Symbol(s"event${eventType.toString}") \ Symbol("members")
  val pathEtmpEventDetails: JsPath = __ \ Symbol("eventDetails")
  val pathUaMembersDetails: JsPath = __ \ Symbol("membersDetails")
  val pathEtmpIndividualDetails: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("individualDetails")
  val pathEtmpDeceasedDetails: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("personReceivedThePayment")

  val pathDeceasedMemberDetails: JsPath = __ \ Symbol("deceasedMembersDetails")
  val pathPersonReceivedThePayment: JsPath = __ \ Symbol("personReceivedThePayment")
  val pathBeneficiaryMemberDetails: JsPath = __ \ Symbol("beneficiaryDetails")
  val pathPaymentDetails: JsPath = __ \ Symbol("paymentDetails")
  val pathPaymentDetailsEvent2: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails")
  val pathPaymentDetailsEvent6: JsPath = __ \ Symbol("AmountCrystallisedAndDate")

  val pathUaChooseTaxYearEvent22And23: JsPath = __ \ Symbol("chooseTaxYear")
  val pathEtmpTaxYearEndingDateEvent22And23: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("taxYearEndingDate")
  val pathUaTotalPensionAmountsEvent22And23: JsPath = __ \ Symbol("totalPensionAmounts")
  val pathEtmpMonetaryAmountEvent22And23: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("monetaryAmount")
  val pathEtmpMonetaryAmountEvent4And5: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("amountPaid")
  val pathEtmpAmountLumpSumEvent7: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("amountLumpSum")
  val pathEtmpAmountCrystallisedEvent7: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("amountCrystalised")
  val pathEtmpDateEvent7: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("eventDate")
  val pathEtmpDateEvent4And5: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("eventDate")
  val pathEtmpTypeOfProtectionEvent6: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("typeOfProtection")
  val pathEtmpInputProtectionType: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("freeText")
  val pathEtmpAmountCrystallisedEvent6: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("amountCrystalised")
  val pathEtmpEventDateEvent6: JsPath = __ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("eventDate")
}

//noinspection ScalaStyle
private object ReadsWithTransform extends Transformer {
  import transformations.ETMPToFrontEnd.Paths._

  lazy val readsMemberDetails: Reads[JsObject] = {(
    (pathUaMembersDetails \ Symbol("firstName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("firstName")).json.pick) and
      (pathUaMembersDetails \ Symbol("lastName")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("lastName")).json.pick) and
      (pathUaMembersDetails \ Symbol("nino")).json.copyFrom((pathEtmpIndividualDetails \ Symbol("nino")).json.pick)
    ).reduce
  }

  lazy val readsTaxYearEndDateEvent22And23: Reads[JsString] = {
    pathEtmpTaxYearEndingDateEvent22And23.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString((str.substring(0,4).toInt - 1).toString))
      case _ => fail[JsString]
    }
  }
}
