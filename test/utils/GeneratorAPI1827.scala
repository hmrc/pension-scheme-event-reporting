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

package utils

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}

trait GeneratorAPI1827 extends Matchers with OptionValues with ResponseGenerators {
  private val benefitInKind = "benefitInKind"
  private val transferToNonRegPensionScheme = "transferToNonRegPensionScheme"
  private val errorCalcTaxFreeLumpSums = "errorCalcTaxFreeLumpSums"
  private val benefitsPaidEarly = "benefitsPaidEarly"
  private val refundOfContributions = "refundOfContributions"
  private val overpaymentOrWriteOff = "overpaymentOrWriteOff"
  private val residentialPropertyHeldMember = "residentialPropertyHeld"
  private val residentialPropertyHeldEmployer = "residentialProperty"
  private val tangibleMoveablePropertyHeld = "tangibleMoveablePropertyHeld"
  private val tangibleMoveablePropertyHeldEmployer = "tangibleMoveableProperty"
  private val courtOrConfiscationOrder = "courtOrConfiscationOrder"
  private val courtOrConfiscationOrderEmployer = "courtOrder"
  private val other = "memberOther"
  private val otherEmployer = "employerOther"
  private val paymentNatureTypesMember = Map(
    benefitInKind -> "Benefit in kind",
    transferToNonRegPensionScheme -> "Transfer to non-registered pensions scheme",
    errorCalcTaxFreeLumpSums -> "Error in calculating tax free lump sums",
    benefitsPaidEarly -> "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum",
    refundOfContributions -> "Refund of contributions",
    overpaymentOrWriteOff -> "Overpayment of pension/written off",
    residentialPropertyHeldMember -> "Residential property held directly or indirectly by an investment-regulated pension scheme",
    tangibleMoveablePropertyHeld -> "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme",
    courtOrConfiscationOrder -> "Court Order Payment/Confiscation Order",
    other -> "Other"
  )

  private val loansExceeding50PercentOfFundValue = "loansExceeding50PercentOfFundValue"
  private val paymentNatureTypesEmployer = Map(
    loansExceeding50PercentOfFundValue -> "Loans to or in respect of the employer exceeding 50% of the value of the fund",
    residentialPropertyHeldEmployer -> "Residential property held directly or indirectly by an investment-regulated pension scheme",
    tangibleMoveablePropertyHeldEmployer -> "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme",
    courtOrConfiscationOrderEmployer -> "Court Order Payment/Confiscation Order",
    otherEmployer -> "Other"
  )

  private val whoWasTransferMadeToMap = Map("anEmployerFinanced" -> "Transfer to an Employer Financed retirement Benefit scheme (EFRB)",
    "nonRecognisedScheme" -> "Transfer to a non-recognised pension scheme which is not a qualifying overseas pension scheme",
    "other" -> "Overpayment of pension/written off other"
  )

  private val refundOfContributionsMap = Map(
    "widowOrOrphan" -> "Widow and/or orphan",
    "other" -> "Overpayment of pension/written off other"
  )

  private val overpaymentOrWriteOffMap = Map(
    "deathOfMember" -> "Death of member",
    "deathOfDependent" -> "Death of dependent",
    "dependentNoLongerQualifiedForPension" -> "Dependent no longer qualified for pension",
    "other" -> "Overpayment of pension/written off other"
  )


  private def pstrOrReference(paymentNature: String, schemeRef: String): String = paymentNature match {
    case `transferToNonRegPensionScheme` => schemeRef
    case _ => ""
  }

  //scalastyle:off cyclomatic.complexity
  // scalastyle:off method.length
  private def generateMember: Gen[(JsObject, JsObject)] = {
    for {
      firstName <- Gen.alphaStr
      lastName <- Gen.alphaStr
      nino <- Gen.oneOf(Seq("AB123456C", "CD123456E"))
      signedMandate <- arbitrary[Boolean]
      paymentNature <- Gen.oneOf(paymentNatureTypesMember.keys.toSeq)
      unAuthorisedPayment <- arbitrary[Boolean]
      unAuthPaySurcharge <- arbitrary[Boolean]
      benefitInKindDesc <- Gen.alphaStr
      errorDesc <- Gen.alphaStr
      tangibleMoveable <- Gen.alphaStr
      whoWasTransferMadeTo <- Gen.oneOf(whoWasTransferMadeToMap.keys.toSeq)
      refundOfContributionsVal <- Gen.oneOf(refundOfContributionsMap.keys.toSeq)
      overpaymentOrWriteOffVal <- Gen.oneOf(overpaymentOrWriteOffMap.keys.toSeq)
      schemeName <- Gen.alphaStr
      schemeRef <- Gen.alphaStr
      paymentVal <- arbitrary[BigDecimal]
      paymentDate <- dateGenerator
      benefitsPaidEarlyVal <- Gen.alphaStr
      address <- addressGenerator
      unAuthPmtRecipient <- Gen.alphaStr
      otherDesc <- Gen.alphaStr
    } yield {
      val ua = Json.obj(
        "membersDetails" -> Json.obj(
          "firstName" -> firstName,
          "lastName" -> lastName,
          "nino" -> nino
        ),
        "doYouHoldSignedMandate" -> signedMandate,
        "valueOfUnauthorisedPayment" -> unAuthorisedPayment,
        "schemeUnAuthPaySurchargeMember" -> unAuthPaySurcharge,
        "paymentNatureMember" -> paymentNature,
        "benefitInKindBriefDescription" -> benefitInKindDesc,
        "errorDescription" -> errorDesc,
        "memberTangibleMoveableProperty" -> tangibleMoveable,
        "whoWasTheTransferMade" -> whoWasTransferMadeTo,
        refundOfContributions -> refundOfContributionsVal,
        benefitsPaidEarly -> benefitsPaidEarlyVal,
        "reasonForTheOverpaymentOrWriteOff" -> overpaymentOrWriteOffVal,
        "schemeDetails" -> Json.obj(
          "schemeName" -> schemeName,
          "reference" -> schemeRef
        ),
        "paymentValueAndDate" -> Json.obj(
          "paymentValue" -> paymentVal,
          "paymentDate" -> paymentDate
        ),
        "unauthorisedPaymentRecipientName" -> unAuthPmtRecipient,
        "memberPaymentNatureDescription" -> otherDesc,
        "memberResidentialAddress" -> Json.obj(
          "address" -> toUserAnswersFormat(address)
        ),
        "memberStatus" -> "New",
        "amendedVersion" -> "001"
      )

      def freeTxtOrSchemeOrRecipientName = paymentNature match {
        case `benefitInKind` => benefitInKindDesc
        case `errorCalcTaxFreeLumpSums` => errorDesc
        case `transferToNonRegPensionScheme` => schemeName
        case `benefitsPaidEarly` => benefitsPaidEarlyVal
        case `tangibleMoveablePropertyHeld` => tangibleMoveable
        case `courtOrConfiscationOrder` => unAuthPmtRecipient
        case `other` => otherDesc
        case _ => ""
      }

      val unAuthorisedPmtType2: String =
        paymentNature match {
          case `transferToNonRegPensionScheme` => whoWasTransferMadeToMap(whoWasTransferMadeTo)
          case `refundOfContributions` => refundOfContributionsMap(refundOfContributionsVal)
          case `overpaymentOrWriteOff` => overpaymentOrWriteOffMap(overpaymentOrWriteOffVal)
          case _ => ""
        }

      val unAuthPmt2 = paymentNature match {
        case `transferToNonRegPensionScheme` | `refundOfContributions` | `overpaymentOrWriteOff` =>
          Json.obj("unAuthorisedPmtType2" -> unAuthorisedPmtType2)
        case _ => Json.obj()
      }

      val freeTxtOrRecipientNameMember = paymentNature match {
        case `overpaymentOrWriteOff` | `refundOfContributions` | `residentialPropertyHeldMember` =>
          Json.obj()
        case _ => Json.obj("freeTxtOrSchemeOrRecipientName" -> freeTxtOrSchemeOrRecipientName)
      }

      val pstrOrRef = paymentNature match {
        case `transferToNonRegPensionScheme` => Json.obj("pstrOrReference" -> pstrOrReference(paymentNature, schemeRef))
        case _ => Json.obj()
      }

      val residentialPropertyHeldReads = paymentNature match {
        case `residentialPropertyHeldMember` => Json.obj("residentialPropertyAddress" -> toAPIFormat(address))
        case _ => Json.obj()
      }

      val unAuthPaySurchargeValue = if (unAuthorisedPayment) {
        Json.obj("schemePayingSurcharge" -> toYesNo(unAuthPaySurcharge))
      } else {
        Json.obj()
      }

      def unAuthorisedPaymentDetails: JsObject = Json.obj(
        "unAuthorisedPmtType1" -> paymentNatureTypesMember(paymentNature),
        "valueOfUnauthorisedPayment" -> paymentVal,
        "dateOfUnauthorisedPayment" -> paymentDate
      ) ++ unAuthPmt2 ++ freeTxtOrRecipientNameMember ++ pstrOrRef ++ residentialPropertyHeldReads

      val indMembDetails = Json.obj(
        "firstName" -> firstName,
        "lastName" -> lastName,
        "nino" -> nino,
        "signedMandate" -> toYesNo(signedMandate),
        "pmtMoreThan25PerFundValue" -> toYesNo(unAuthorisedPayment),
      ) ++ unAuthPaySurchargeValue

      val expectedJson = Json.obj(
        "individualMemberDetails" -> indMembDetails,
        "unAuthorisedPaymentDetails" -> unAuthorisedPaymentDetails,
        "memberStatus" -> "New",
        "amendedVersion" -> "001"
      )
      Tuple2(ua, expectedJson)
    }
  }

  private def generateEmployer: Gen[(JsObject, JsObject)] = {
    for {
      companyName <- Gen.alphaStr
      companyNumber <- Gen.alphaStr
      address <- addressGenerator
      paymentNature <- Gen.oneOf(paymentNatureTypesEmployer.keys.toSeq)
      loanAmount <- arbitrary[BigDecimal]
      loanValue <- arbitrary[BigDecimal]
      residentialAddress <- addressGenerator
      tangibleMoveableProperyDesc <- Gen.alphaStr
      recipientName <- Gen.alphaStr
      paymentNatureDesc <- Gen.alphaStr
      paymentVal <- arbitrary[BigDecimal]
      paymentDate <- dateGenerator
    } yield {
      val ua = Json.obj(
        "event1" -> Json.obj(
          "companyDetails" -> Json.obj(
            "companyName" -> companyName,
            "companyNumber" -> companyNumber
          )
        ),
        "employerAddress" -> Json.obj(
          "address" -> toUserAnswersFormat(address)
        ),
        "employerResidentialAddress" -> Json.obj(
          "address" -> toUserAnswersFormat(residentialAddress)
        ),
        "paymentValueAndDate" -> Json.obj(
          "paymentValue" -> paymentVal,
          "paymentDate" -> paymentDate
        ),
        "paymentNatureEmployer" -> paymentNature,
        "loanDetails" -> Json.obj(
          "loanAmount" -> loanAmount,
          "fundValue" -> loanValue
        ),
        "employerTangibleMoveableProperty" -> tangibleMoveableProperyDesc,
        "unauthorisedPaymentRecipientName" -> recipientName,
        "paymentNatureDesc" -> paymentNatureDesc,
        "memberStatus" -> "New",
        "amendedVersion" -> "001"
      )

      def freeTxtOrSchemeOrRecipientName: Option[String] = paymentNature match {
        case `tangibleMoveablePropertyHeldEmployer` => Some(tangibleMoveableProperyDesc)
        case `courtOrConfiscationOrderEmployer` => Some(recipientName)
        case `otherEmployer` => Some(paymentNatureDesc)
        case _ => None
      }

      val freeTextOrSchemeOrRecipientName = paymentNature match {
        case `overpaymentOrWriteOff` | `refundOfContributions` | `residentialPropertyHeldEmployer` =>
          Json.obj()
        case _ => freeTxtOrSchemeOrRecipientName.fold(Json.obj()) { ft =>
          Json.obj("freeTxtOrSchemeOrRecipientName" -> ft)
        }
      }

      val loanOptionFundValue = paymentNature match {
        case `loansExceeding50PercentOfFundValue` => Json.obj("fundValue" -> loanValue)
        case _ =>
          Json.obj()
      }

      val paymentOrLoanAmountVal = paymentNature match {
        case `loansExceeding50PercentOfFundValue` | `courtOrConfiscationOrderEmployer` =>
          Json.obj(
            "pmtAmtOrLoanAmt" -> loanAmount
          ) ++ loanOptionFundValue
        case _ =>
          Json.obj()
      }

      val residentialPropertyAddressEmployer = paymentNature match {
        case `residentialPropertyHeldEmployer` => Json.obj("residentialPropertyAddress" -> toAPIFormat(residentialAddress))
        case _ => Json.obj()
      }

      def unauthorisedPaymentDetails: JsObject = Json.obj(
        "unAuthorisedPmtType1" -> paymentNatureTypesEmployer(paymentNature),
        "valueOfUnauthorisedPayment" -> paymentVal,
        "dateOfUnauthorisedPayment" -> paymentDate
      ) ++ freeTextOrSchemeOrRecipientName ++ residentialPropertyAddressEmployer ++ paymentOrLoanAmountVal

      val expectedJson = Json.obj(
        "employerMemDetails" -> Json.obj(
          "compOrOrgName" -> companyName,
          "crnNumber" -> companyNumber,
          "addressDetails" -> toAPIFormat(address)
        ),
        "unAuthorisedPaymentDetails" ->
          unauthorisedPaymentDetails,
        "memberStatus" -> "New",
        "amendedVersion" -> "001"
      )
      Tuple2(ua, expectedJson)
    }
  }

  def generateUserAnswersAndPOSTBody: Gen[Tuple2[JsObject, JsObject]] = {
    val whoReceivedUnauthorisedPaymentMember = "member"
    val whoReceivedUnauthorisedPaymentEmployer = "employer"
    val whoReceivedUnauthorisedPaymentMap: Map[String, String] = Map(
      whoReceivedUnauthorisedPaymentMember -> "Individual",
      whoReceivedUnauthorisedPaymentEmployer -> "Employer",
    )

     val result = for {
      taxYear <- taxYearGenerator
      whoReceivedUnauthorisedPayment <- Gen.oneOf(whoReceivedUnauthorisedPaymentMember, whoReceivedUnauthorisedPaymentEmployer)
    } yield {
      (whoReceivedUnauthorisedPayment match {
        case `whoReceivedUnauthorisedPaymentMember` => generateMember
        case _ => generateEmployer
      }).map { case (generatedUA, generatedExpectedResult) =>
        val fullUA = Json.obj(
          "event1" ->
            Json.obj(
              "membersOrEmployers" ->
                Json.arr(
                  generatedUA ++ Json.obj("whoReceivedUnauthPayment" -> whoReceivedUnauthorisedPayment)
                )
            ),
          "taxYear" -> taxYear
        )
        val endTaxYear = (taxYear.toInt + 1).toString
        val fullExpectedResult = Json.obj(
          "eventReportDetails" -> Json.obj(
            "reportStartDate" -> s"$taxYear-04-06",
            "reportEndDate" -> s"$endTaxYear-04-05"
          ),
          "event1Details" -> Json.obj(
            "event1Details" -> Json.arr(
              generatedExpectedResult ++ Json.obj(
                "memberType" -> whoReceivedUnauthorisedPaymentMap(whoReceivedUnauthorisedPayment)
              )
            )
          )
        )
        Tuple2(fullUA, fullExpectedResult)
      }

    }

    result.flatMap(identity)

  }
}
