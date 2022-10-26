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

package utils

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}

trait GeneratorAPI1827 extends Matchers with OptionValues with ResponseGenerators {
  private val paymentNatureTypesMember = Map(
    "benefitInKind" -> "Benefit in kind",
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

  private val paymentNatureTypesEmployer = Map(
    "loansExceeding50PercentOfFundValue" -> "Loans to or in respect of the employer exceeding 50% of the value of the fund",
    "residentialPropertyHeld" -> "Residential property held directly or indirectly by an investment-regulated pension scheme",
    "tangibleMoveablePropertyHeld" -> "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme",
    "courtOrConfiscationOrder" -> "Court Order Payment/Confiscation Order",
    "other" -> "Other"
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

  //scalastyle:off
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
      refundOfContributions <- Gen.oneOf(refundOfContributionsMap.keys.toSeq)
      overpaymentOrWriteOff <- Gen.oneOf(overpaymentOrWriteOffMap.keys.toSeq)
      schemeName <- Gen.alphaStr
      schemeRef <- Gen.alphaStr
      paymentVal <- arbitrary[BigDecimal]
      paymentDate <- dateGenerator
      benefitsPaidEarly <- Gen.alphaStr
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
        "paymentNature" -> paymentNature,
        "benefitInKindBriefDescription" -> benefitInKindDesc,
        "errorDescription" -> errorDesc,
        "memberTangibleMoveableProperty" -> tangibleMoveable,
        "whoWasTheTransferMade" -> whoWasTransferMadeTo,
        "refundOfContributions" -> refundOfContributions,
        "benefitsPaidEarly" -> benefitsPaidEarly,
        "reasonForTheOverpaymentOrWriteOff" -> overpaymentOrWriteOff,
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
        "event1" -> Json.obj(
          "memberResidentialAddress" -> Json.obj(
            "address" -> address.toUA
          )
        )
      )

      def freeTxtOrSchemeOrRecipientName = paymentNature match {
        case "benefitInKind" => benefitInKindDesc
        case "errorCalcTaxFreeLumpSums" => errorDesc
        case "transferToNonRegPensionScheme" => schemeName
        case "benefitsPaidEarly" => benefitsPaidEarly
        case "tangibleMoveablePropertyHeld" => tangibleMoveable
        case "courtOrConfiscationOrder" => unAuthPmtRecipient
        case "other" => otherDesc
        case _ => ""
      }

      val unAuthorisedPmtType2: String =
        paymentNature match {
          case "transferToNonRegPensionScheme" => whoWasTransferMadeToMap(whoWasTransferMadeTo)
          case "refundOfContributions" => refundOfContributionsMap(refundOfContributions)
          case "overpaymentOrWriteOff" => overpaymentOrWriteOffMap(overpaymentOrWriteOff)
          case _ => ""
        }

      val unAuthPmt2 = paymentNature match {
        case "transferToNonRegPensionScheme" | "refundOfContributions" | "overpaymentOrWriteOff" =>
          Json.obj("unAuthorisedPmtType2" -> unAuthorisedPmtType2)
        case _ => Json.obj()
      }

      val freeTxtOrRecipientNameMember = paymentNature match {
        case "overpaymentOrWriteOff" | "refundOfContributions" | "residentialPropertyHeld" =>
          Json.obj()
        case _ => Json.obj("freeTxtOrSchemeOrRecipientName" -> freeTxtOrSchemeOrRecipientName)
      }

      val pstrOrRef = paymentNature match {
        case "transferToNonRegPensionScheme" => Json.obj("pstrOrReference" -> pstrOrReference(paymentNature, schemeRef))
        case _ => Json.obj()
      }

      val residentialPropertyHeld = paymentNature match {
        case "residentialPropertyHeld" => Json.obj("residentialPropertyAddress" -> address.toTarget)
        case _ => Json.obj()
      }

      val unAuthPaySurchargeValue = unAuthorisedPayment match {
        case true => Json.obj("schemePayingSurcharge" -> toYesNo(unAuthPaySurcharge))
        case _ => Json.obj()
      }

      def unAuthorisedPaymentDetails: JsObject = Json.obj(
        "unAuthorisedPmtType1" -> paymentNatureTypesMember(paymentNature),
        "valueOfUnauthorisedPayment" -> paymentVal,
        "dateOfUnauthorisedPayment" -> paymentDate
      ) ++ unAuthPmt2 ++ freeTxtOrRecipientNameMember ++ pstrOrRef ++ residentialPropertyHeld

      val indMembDetails = Json.obj(
        "firstName" -> firstName,
        "lastName" -> lastName,
        "nino" -> nino,
        "signedMandate" -> toYesNo(signedMandate),
        "pmtMoreThan25PerFundValue" -> toYesNo(unAuthorisedPayment),
      ) ++ unAuthPaySurchargeValue

      val expectedJson = Json.obj(
        "individualMemberDetails" -> indMembDetails,
        "unAuthorisedPaymentDetails" ->
          unAuthorisedPaymentDetails
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
          ),
          "employerAddress" -> Json.obj(
            "address" -> address.toUA
          ),
          "employerResidentialAddress" -> Json.obj(
            "address" -> residentialAddress.toUA
          )
        ),
        "paymentValueAndDate" -> Json.obj(
          "paymentValue" -> paymentVal,
          "paymentDate" -> paymentDate
        ),
        "paymentNature" -> paymentNature,
        "loanDetails" -> Json.obj(
          "loanAmount" -> loanAmount,
          "fundValue" -> loanValue
        ),
        "employerTangibleMoveableProperty" -> tangibleMoveableProperyDesc,
        "unauthorisedPaymentRecipientName" -> recipientName,
        "paymentNatureDesc" -> paymentNatureDesc
      )

      def freeTxtOrSchemeOrRecipientName: Option[String] = paymentNature match {
        case "tangibleMoveablePropertyHeld" => Some(tangibleMoveableProperyDesc)
        case "courtOrConfiscationOrder" => Some(recipientName)
        case "other" => Some(paymentNatureDesc)
        case _ => None
      }

      val freeTextOrSchemeOrRecipientName = paymentNature match {
        case "overpaymentOrWriteOff" | "refundOfContributions" | "residentialPropertyHeld" =>
          Json.obj()
        case _ => freeTxtOrSchemeOrRecipientName.fold(Json.obj()) { ft =>
          Json.obj("freeTxtOrSchemeOrRecipientName" -> ft)
        }
      }

      val loanOptionFundValue = paymentNature match {
        case "loansExceeding50PercentOfFundValue" => Json.obj("fundValue" -> loanValue)
        case _ =>
          Json.obj()
      }

      val xx = paymentNature match {
        case "loansExceeding50PercentOfFundValue" | "courtOrConfiscationOrder" =>
          Json.obj(
            "pmtAmtOrLoanAmt" -> loanAmount
          ) ++ loanOptionFundValue
        case _ =>
          Json.obj()
      }

      val residentialPropertyAddressEmployer = paymentNature match {
        case "residentialPropertyHeld" => Json.obj("residentialPropertyAddress" -> residentialAddress.toTarget)
        case _ => Json.obj()
      }

      def unauthorisedPaymentDetails: JsObject = Json.obj(
        "unAuthorisedPmtType1" -> paymentNatureTypesEmployer(paymentNature),
        "valueOfUnauthorisedPayment" -> paymentVal,
        "dateOfUnauthorisedPayment" -> paymentDate
      ) ++ freeTextOrSchemeOrRecipientName ++ residentialPropertyAddressEmployer ++ xx

      val expectedJson = Json.obj(
        "employerMemDetails" -> Json.obj(
          "compOrOrgName" -> companyName,
          "crnNumber" -> companyNumber,
          "addressDetails" -> address.toTarget
        ),
        "unAuthorisedPaymentDetails" ->
          unauthorisedPaymentDetails
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
    Gen.oneOf(whoReceivedUnauthorisedPaymentMember, whoReceivedUnauthorisedPaymentEmployer)
      .flatMap { whoReceivedUnauthorisedPayment =>
        (whoReceivedUnauthorisedPayment match {
          case `whoReceivedUnauthorisedPaymentMember` => generateMember
          case _ => generateEmployer
        }).map { case (generatedUA, generatedExpectedResult) =>
          val fullUA = Json.obj(
            "membersOrEmployers" ->
              Json.arr(
                generatedUA ++ Json.obj("whoReceivedUnauthPayment" -> whoReceivedUnauthorisedPayment)
              )
          )
          val fullExpectedResult = Json.obj(
            "eventReportDetails" -> Json.obj(
              "reportStartDate" -> "2020-09-01",
              "reportEndDate" -> "2020-09-01"
            ),
            "event1Details" -> Json.obj(
              "event1Details" -> Json.arr(
                generatedExpectedResult ++ Json.obj(
                  "memberType" -> whoReceivedUnauthorisedPaymentMap(whoReceivedUnauthorisedPayment)
                ) ++ Json.obj(
                  "memberStatus" -> "New"
                )
              )
            )
          )
          Tuple2(fullUA, fullExpectedResult)
        }
      }
  }


  private def pstrOrReference(paymentNature: String, schemeRef: String): String = paymentNature match {
    case "transferToNonRegPensionScheme" => schemeRef
    case _ => ""
  }
}