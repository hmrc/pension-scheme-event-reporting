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

package transformations

import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._

trait Transformer {
  protected val doNothing: Reads[JsObject] = __.json.put(Json.obj())
  protected def fail[A]: Reads[A] = Reads.failed[A]("Unknown value")

  protected def fail[A](s: A): Reads[A] = Reads.failed[A](s"Unknown value: $s")

  protected def toYesNo(b: JsValue): JsString = if (b.as[JsBoolean].value) JsString("Yes") else JsString("No")

  lazy val yesNoTransformToBoolean: String => Boolean = {
    case "Yes" => true
    case "No" => false
  }

  protected val yes: JsString = JsString("Yes")

  protected def readsAddress(jsPath: JsPath): Reads[JsObject] =
    (
      (jsPath \ Symbol("address") \ Symbol("addressLine1")).read[String] and
        (jsPath \ Symbol("address") \ Symbol("addressLine2")).read[String] and
        (jsPath \ Symbol("address") \ Symbol("addressLine3")).readNullable[String] and
        (jsPath \ Symbol("address") \ Symbol("addressLine4")).readNullable[String] and
        (jsPath \ Symbol("address") \ Symbol("postcode")).readNullable[String] and
        (jsPath \ Symbol("address") \ Symbol("country")).read[String]
      ) (
      (addressLine1, addressLine2, addressLine3, addressLine4, postcode, country) =>
        Json.obj(
          "addressLine1" -> addressLine1,
          "addressLine2" -> addressLine2,
          "countryCode" -> country
        ) ++ addressLine3.fold(Json.obj()) { addr =>
          Json.obj("addressLine3" -> addr)
        } ++
          addressLine4.fold(Json.obj()) { addr =>
            Json.obj("addressLine3" -> addr)
          } ++
          postcode.fold(Json.obj()) { postcode =>
            Json.obj("postCode" -> postcode)
          }
    )


  protected def readsAddressEtmp(jsPathEtmp: JsPath): Reads[JsObject] =
    (
      (jsPathEtmp \  Symbol("addressLine1")).read[String] and
        (jsPathEtmp \  Symbol("addressLine2")).read[String] and
        (jsPathEtmp \  Symbol("addressLine3")).readNullable[String] and
        (jsPathEtmp \  Symbol("addressLine4")).readNullable[String] and
        (jsPathEtmp \  Symbol("postCode")).readNullable[String] and
        (jsPathEtmp \  Symbol("countryCode")).read[String]
      )(
      (addressLine1, addressLine2, addressLine3, addressLine4, postcode, country) =>
        Json.obj(
          "addressLine1" -> addressLine1,
          "addressLine2" -> addressLine2,
          "country" -> country
        ) ++ addressLine3.fold(Json.obj()) { addr =>
          Json.obj("addressLine3" -> addr)
        } ++
          addressLine4.fold(Json.obj()) { addr =>
            Json.obj("addressLine3" -> addr)
          } ++
          postcode.fold(Json.obj()) { postcode =>
            Json.obj("postcode" -> postcode)
          }
    )

  def readAdditiveIfPresent(etmpPath: JsPath, uaPath: JsPath, f: String => String = identity): JsObject => Reads[JsObject] = jsObj =>
    etmpPath.readNullable[String].flatMap {
      case None => Reads.pure(jsObj)
      case Some(str) => uaPath.json.put(JsString(f(str))).map(_ ++ jsObj)
    }

  def typeOfProtectionUAEvent6(tOP: String): String = tOP match {
    case "Enhanced life time allowance" => "enhancedLifetimeAllowance"
    case "Enhanced protection" => "enhancedProtection"
    case "Fixed protection" => "fixedProtection"
    case "Fixed protection 2014" => "fixedProtection2014"
    case "Fixed protection 2016" => "fixedProtection2016"
    case "Individual protection 2014" => "individualProtection2014"
    case "Individual protection 2016" => "individualProtection2016"
  }

  def paymentTypeUAEvent8A(rBT: String): String = rBT match {
    case "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection"
    => "paymentOfAStandAloneLumpSum"
    case "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance"
    => "paymentOfASchemeSpecificLumpSum"
  }

  def typeOfProtectionUAEvent8(tOP: String): String = tOP match {
    case "Primary Protection" => "primaryProtection"
    case "Enhanced protection" => "enhancedProtection"
  }

  def typeOfProtectionUAEvent8A(tOP: String): String = tOP match {
    case "Primary Protection" => "primaryProtection"
    case "Enhanced protection" => "enhancedProtection"
  }

  def reasonBenefitTakenUAEvent3(rBT: String): String = rBT match {
    case "Ill Health" => "illHealth"
    case "Protected Pension Age" => "protectedPensionAge"
    case "Other" => "other"
  }

  def typeOfProtectionUAEvent24(tOP: String): String = tOP match {
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

  def bceTypeUAEvent24(tOP: String): String = tOP match {
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

  lazy val memberTypeTransform: String => String = {
    case "Individual" => "member"
    case "Employer" => "employer"
  }

  val unAuthorisedPmtType1IndividualTransform: String => String = {
    case "Benefit in kind" => "benefitInKind"
    case "Transfer to non-registered pensions scheme" => "transferToNonRegPensionScheme"
    case "Error in calculating tax free lump sums" => "errorCalcTaxFreeLumpSums"
    case "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum" => "benefitsPaidEarly"
    case "Refund of contributions" => "refundOfContributions"
    case "Overpayment of pension/written off" => "overpaymentOrWriteOff"
    case "Loans to or in respect of the employer exceeding 50% of the value of the fund" => "loansExceeding50PercentOfFundValue"
    case "Residential property held directly or indirectly by an investment-regulated pension scheme" => "residentialPropertyHeld"
    case "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme" => "tangibleMoveablePropertyHeld"
    case "Court Order Payment/Confiscation Order" => "courtOrConfiscationOrder"
    case "Other" => "memberOther"
  }

  val unAuthorisedPmtType1EmployerTransform: String => String = {
    case "Benefit in kind" => "benefitInKind"
    case "Transfer to non-registered pensions scheme" => "transferToNonRegPensionScheme"
    case "Error in calculating tax free lump sums" => "errorCalcTaxFreeLumpSums"
    case "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum" => "benefitsPaidEarly"
    case "Refund of contributions" => "refundOfContributions"
    case "Overpayment of pension/written off" => "overpaymentOrWriteOff"
    case "Loans to or in respect of the employer exceeding 50% of the value of the fund" => "loansExceeding50PercentOfFundValue"
    case "Residential property held directly or indirectly by an investment-regulated pension scheme" => "residentialProperty"
    case "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme" => "tangibleMoveableProperty"
    case "Court Order Payment/Confiscation Order" => "courtOrder"
    case "Other" => "employerOther"
  }

  lazy val unAuthorisedPmtType2Transform: String => String = {
    case "Transfer to an Employer Financed retirement Benefit scheme (EFRB)" => "anEmployerFinanced"
    case "Transfer to a non-recognised pension scheme which is not a qualifying overseas pension scheme" => "nonRecognisedScheme"
    case "Widow and/or orphan" => "widowOrOrphan"
    case "Refund of contributions other" => "other"
    case "Death of member" => "deathOfMember"
    case "Death of dependent" => "deathOfDependent"
    case "Dependent no longer qualified for pension" => "dependentNoLongerQualifiedForPension"
    case "Overpayment of pension/written off other" => "other"
    case _ => ""
  }

  def event3TypeOfBenefitConversion(tOB: String): String = tOB match {
    case "illHealth" => "Ill Health"
    case "protectedPensionAge" => "Protected Pension Age"
    case "other" => "Other"
  }

  def event6TypeOfProtectionConversion(tOP: String): String = tOP match {
    case "enhancedLifetimeAllowance" => "Enhanced life time allowance"
    case "enhancedProtection" => "Enhanced protection"
    case "fixedProtection" => "Fixed protection"
    case "fixedProtection2014" => "Fixed protection 2014"
    case "fixedProtection2016" => "Fixed protection 2016"
    case "individualProtection2014" => "Individual protection 2014"
    case "individualProtection2016" => "Individual protection 2016"
  }

  def event8TypeOfProtectionConversion(tOP: String): String = tOP match {
    case "primaryProtection" => "Primary Protection"
    case "enhancedProtection" => "Enhanced protection"
  }

  def event8ATypeOfProtectionConversion(tOP: String): String = tOP match {
    case "primaryProtection" => "Primary Protection"
    case "enhancedProtection" => "Enhanced protection"
  }

  //noinspection ScalaStyle
  def event8APaymentTypeConversion(pT: String): String = pT match {
    case "paymentOfAStandAloneLumpSum" =>
      "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection"
    case "paymentOfASchemeSpecificLumpSum" =>
      "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance"
  }

  def event24TypeOfProtectionGroup2Conversion(tOP: String): String = tOP match {
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
  def event24ReasonBenefitTakenConversion(tOB: String): String = tOB match {
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

  val paymentNatureTypeKeyBenefitInKind: String = "benefitInKind"
  val paymentNatureTypeKeyTransferToNonRegPensionScheme: String = "transferToNonRegPensionScheme"
  val paymentNatureTypeKeyRefundOfContributions: String = "refundOfContributions"
  val paymentNatureTypeKeyOverpaymentOrWriteOff: String = "overpaymentOrWriteOff"
  val paymentNatureTypeKeyResidentialPropertyHeld: String = "residentialPropertyHeld"
  val paymentNatureTypeKeyResidentialPropertyHeldEmployer: String = "residentialProperty"
  val paymentNatureTypeKeyTangibleMoveablePropertyHeld: String = "tangibleMoveablePropertyHeld"
  val paymentNatureTypeKeyTangibleMoveablePropertyHeldEmployer: String = "tangibleMoveableProperty"
  val paymentNatureTypeKeyErrorCalcTaxFreeLumpSums: String = "errorCalcTaxFreeLumpSums"
  val paymentNatureTypeKeyCourtOrConfiscationOrder: String = "courtOrConfiscationOrder"
  val paymentNatureTypeKeyCourtOrConfiscationOrderEmployer: String = "courtOrder"
  val paymentNatureTypeKeyLoansExceeding50PercentOfFundValue: String = "loansExceeding50PercentOfFundValue"
  val paymentNatureTypeKeyOther: String = "memberOther"
  val paymentNatureTypeKeyOtherEmployer: String = "employerOther"
  val paymentNatureTypeKeyBenefitsPaidEarly: String = "benefitsPaidEarly"
  val whoReceivedUnauthPaymentIndividual = "Individual"
  val whoReceivedUnauthPaymentEmployer = "Employer"

  val paymentNatureMemberMap: Map[String, String] = Map(
    paymentNatureTypeKeyBenefitInKind -> "Benefit in kind",
    paymentNatureTypeKeyTransferToNonRegPensionScheme -> "Transfer to non-registered pensions scheme",
    paymentNatureTypeKeyErrorCalcTaxFreeLumpSums -> "Error in calculating tax free lump sums",
    paymentNatureTypeKeyBenefitsPaidEarly -> "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum",
    paymentNatureTypeKeyRefundOfContributions -> "Refund of contributions",
    paymentNatureTypeKeyOverpaymentOrWriteOff -> "Overpayment of pension/written off",
    paymentNatureTypeKeyResidentialPropertyHeld -> "Residential property held directly or indirectly by an investment-regulated pension scheme",
    paymentNatureTypeKeyTangibleMoveablePropertyHeld -> "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme",
    paymentNatureTypeKeyCourtOrConfiscationOrder -> "Court Order Payment/Confiscation Order",
    paymentNatureTypeKeyOther -> "Other"
  )

  val paymentNatureEmployerMap: Map[String, String] = Map(
    paymentNatureTypeKeyLoansExceeding50PercentOfFundValue -> "Loans to or in respect of the employer exceeding 50% of the value of the fund",
    paymentNatureTypeKeyResidentialPropertyHeldEmployer -> "Residential property held directly or indirectly by an investment-regulated pension scheme",
    paymentNatureTypeKeyTangibleMoveablePropertyHeldEmployer ->
      "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme",
    paymentNatureTypeKeyCourtOrConfiscationOrderEmployer -> "Court Order Payment/Confiscation Order",
    paymentNatureTypeKeyOtherEmployer -> "Other"
  )

  val whoWasTransferMadeToMap: Map[String, String] = Map(
    "anEmployerFinanced" -> "Transfer to an Employer Financed retirement Benefit scheme (EFRB)",
    "nonRecognisedScheme" -> "Transfer to a non-recognised pension scheme which is not a qualifying overseas pension scheme",
    "other" -> "Overpayment of pension/written off other"
  )

  val refundOfContributionsMap: Map[String, String] = Map(
    "widowOrOrphan" -> "Widow and/or orphan",
    "other" -> "Overpayment of pension/written off other"
  )

  val overpaymentOrWriteOffMap: Map[String, String] = Map(
    "deathOfMember" -> "Death of member",
    "deathOfDependent" -> "Death of dependent",
    "dependentNoLongerQualifiedForPension" -> "Dependent no longer qualified for pension",
    "other" -> "Overpayment of pension/written off other"
  )

  def event13SchemeStructureTransformer(schemeStructure: JsValue): JsString = {
    schemeStructure.as[JsString].value match {
      case "single" => JsString("A single trust under which all of the assets are held for the benefit of all members of the scheme")
      case "group" => JsString("A group life/death in service scheme")
      case "corporate" => JsString("A body corporate")
      case _ => JsString("Other")
    }
  }

}
