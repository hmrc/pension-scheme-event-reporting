/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.libs.json.{__, _}

object Event1Details {

  private val paymentNatureTypesMember = Map("benefitInKind" -> "Benefit in kind",
    "transferToNonRegPensionScheme" -> "Transfer to non-registered pensions scheme",
    "errorCalcTaxFreeLumpSums" -> "Error in calculating tax free lump sums",
    "benefitsPaidEarly" -> "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum",
    "refundOfContributions" -> "Refund of contributions",
    "overpaymentOrWriteOff" -> "Overpayment of pension/written off",
    "residentialPropertyHeld" -> "Residential property held directly or indirectly by an investment-regulated pension scheme",
    "tangibleMoveablePropertyHeld" -> "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme",
    "courtOrConfiscationOrder" -> "Court Order Payment/Confiscation Order",
    "other" -> "Other"
  )

  private val whoWasTransferMadeToMap = Map("anEmployerFinanced" -> "Transfer to an Employer Financed retirement Benefit scheme (EFRB)",
    "nonRecognisedScheme" -> "Transfer to a non-recognised pension scheme which is not a qualifying overseas pension scheme",
    "other" -> "Overpayment of pension/written off other")

  private val readsTransferMade: Reads[JsString] =
    (__ \ 'whoWasTheTransferMade).json.pick.map(jsValue => JsString(whoWasTransferMadeToMap(jsValue.as[JsString].value)))

  private def freeTxtOrSchemeOrRecipientName(paymentNature: String): Reads[JsString] = {
    paymentNature match {
      case "benefitInKind" => (__ \ 'benefitInKindBriefDescription).json.pick.map(_.as[JsString])
      case "transferToNonRegPensionScheme" => (__ \ 'schemeDetails \ 'schemeName).json.pick.map(_.as[JsString])
      case "errorCalcTaxFreeLumpSums" => (__ \ 'errorDescription).json.pick.map(_.as[JsString])
      case _ => Reads[JsString](_ => JsSuccess(JsString("")))
    }
  }

  private def pstrOrReference(paymentNature: String): Reads[JsString] = {
    paymentNature match {
      case "transferToNonRegPensionScheme" => (__ \ 'schemeDetails \ 'reference).json.pick.map(_.as[JsString])
      case _ => Reads[JsString](_ => JsError(""))
    }
  }

  private val doNothing: Reads[JsObject] = __.json.put(Json.obj())

  private val readsPaymentNature: Reads[String] = (__ \ 'paymentNature).json.pick.map(_.as[JsString].value)

  private val readsMember: Reads[JsObject] = {
    (for {
      paymentNature <- readsPaymentNature
    } yield {
      (
        (__ \ 'individualMemberDetails \ 'firstName).json.copyFrom((__ \ 'membersDetails \ 'firstName).json.pick) and
          (__ \ 'individualMemberDetails \ 'lastName).json.copyFrom((__ \ 'membersDetails \ 'lastName).json.pick) and
          (__ \ 'individualMemberDetails \ 'nino).json.copyFrom((__ \ 'membersDetails \ 'nino).json.pick) and
          (__ \ 'individualMemberDetails \ 'signedMandate).json.copyFrom((__ \ 'doYouHoldSignedMandate).json.pick) and
          (__ \ 'individualMemberDetails \ 'pmtMoreThan25PerFundValue).json.copyFrom((__ \ 'valueOfUnauthorisedPayment).json.pick) and
          (__ \ 'individualMemberDetails \ 'schemePayingSurcharge).json.copyFrom((__ \ 'schemeUnAuthPaySurchargeMember).json.pick) and
          (__ \ 'unAuthorisedPaymentDetails \ 'unAuthorisedPmtType1).json.put(JsString(paymentNatureTypesMember(paymentNature))) and
          (__ \ 'unAuthorisedPaymentDetails \ 'freeTxtOrSchemeOrRecipientName).json.copyFrom(freeTxtOrSchemeOrRecipientName(paymentNature)) and
          (__ \ 'unAuthorisedPaymentDetails \ 'pstrOrReference).json.copyFrom(pstrOrReference(paymentNature)).orElse(doNothing) and
          (__ \ 'unAuthorisedPaymentDetails \ 'unAuthorisedPmtType2).json.copyFrom(readsTransferMade)
        ).reduce
    }).flatMap[JsObject](identity)
  }

  private def readsMembers: Reads[JsArray] = __.read(Reads.seq(readsMember)).map(JsArray(_))

  def transformToETMPData: Reads[JsObject] = {
    (__ \ 'membersOrEmployers).readNullable[JsArray](readsMembers).map { optionJsArray =>
      val jsonArray = optionJsArray.getOrElse(Json.arr())
      Json.obj("event1Details" ->
        Json.obj("event1Details" -> jsonArray)
      )
    }
  }
}

