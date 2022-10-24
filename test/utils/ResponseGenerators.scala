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

import java.time.LocalDate

trait ResponseGenerators extends Matchers with OptionValues {
  val ninoGen: Gen[String] = Gen.oneOf(Seq("AB123456C", "CD123456E"))

  private val paymentNatureTypesMember = Map(
    "benefitInKind" -> "Benefit in kind",
    "transferToNonRegPensionScheme" -> "Transfer to non-registered pensions scheme",
    "errorCalcTaxFreeLumpSums" -> "Error in calculating tax free lump sums",
    "benefitsPaidEarly" -> "Benefits paid early other than on the grounds of ill-health, protected pension age or a winding up lump sum",
    "refundOfContributions" -> "Refund of contributions",
    "overpaymentOrWriteOff" -> "Overpayment of pension/written off"
    //    "residentialPropertyHeld" -> "Residential property held directly or indirectly by an investment-regulated pension scheme",
    //    "tangibleMoveablePropertyHeld" -> "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme",
    //    "courtOrConfiscationOrder" -> "Court Order Payment/Confiscation Order",
    //    "other" -> "Other"
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

  /*
  "Transfer to an Employer Financed retirement Benefit scheme (EFRB)"
"Transfer to a non-recognised pension scheme which is not a qualifying overseas pension scheme"
"Widow and/or orphan"
"Refund of contributions other"
"Death of member"
"Death of dependent"
"Dependent no longer qualified for pension"
"Overpayment of pension/written off other"
   */

  val dateGenerator: Gen[LocalDate] = for {
    day <- Gen.choose(1, 28)
    month <- Gen.choose(1, 12)
    year <- Gen.choose(1990, 2000)
  } yield LocalDate.of(year, month, day)


  val addressGenerator: Gen[JsObject] = for {
    nonUkAddress <- arbitrary[Boolean]
    line1 <- nonEmptyString
    line2 <- nonEmptyString
    line3 <- Gen.option(nonEmptyString)
    line4 <- Gen.option(nonEmptyString)
    postalCode <- Gen.option(nonEmptyString)
    country <- Gen.listOfN(2, nonEmptyString).map(_.mkString)
  } yield {
    Json.obj(
      fields = "nonUKAddress" -> nonUkAddress.toString.capitalize,
      "addressLine1" -> line1,
      "addressLine2" -> line2,
      "addressLine3" -> line3,
      "addressLine4" -> line4,
      "postCode" -> postalCode,
      "countryCode" -> country
    )
  }

  def nonEmptyString: Gen[String] = Gen.alphaStr.suchThat(!_.isEmpty)

  def generateRandomPayloadAPI1834: Gen[Tuple2[JsObject, Seq[String]]] = {
    val sortEventTypes: (String, String) => Boolean = (a, b) => {
      (a, b) match {
        case ("0", _) => false
        case (_, "0") => true
        case (a, b) if a < b => true
        case _ => false
      }
    }
    val version = "001"
    for {
      chosenEventTypesWithSeq <- Gen.someOf[String](Seq("10", "13", "19", "20"))
      chosenEventTypesWithoutSeq <- Gen.someOf[String](Seq("11", "12", "14", "0"))
    } yield {
      val payloadWithSeq = chosenEventTypesWithSeq.foldLeft(Json.obj()) { (acc, s) =>
        acc ++ Json.obj(
          s"event$s" -> Json.arr(
            Json.obj(
              "recordVersion" -> version
            )
          )
        )
      }
      val payloadWithoutSeq = chosenEventTypesWithoutSeq.foldLeft(Json.obj()) { (acc, s) =>
        acc ++ Json.obj(
          s"""event${if (s == "0") "WindUp" else s}""" ->
            Json.obj(
              "recordVersion" -> version
            )
        )
      }

      val generatedPayload = Json.obj(
        "eventDetails" -> (payloadWithSeq ++ payloadWithoutSeq)
      )

      val expectedEventTypes = (chosenEventTypesWithSeq ++ chosenEventTypesWithoutSeq)
        .sortWith(sortEventTypes)

      Tuple2(generatedPayload, expectedEventTypes)
    }
  }


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
      whoWasTransferMadeTo <- Gen.oneOf(whoWasTransferMadeToMap.keys.toSeq)
      refundOfContributions <- Gen.oneOf(refundOfContributionsMap.keys.toSeq)
      overpaymentOrWriteOff <- Gen.oneOf(overpaymentOrWriteOffMap.keys.toSeq)
      schemeName <- Gen.alphaStr
      schemeRef <- Gen.alphaStr
      paymentVal <- arbitrary[BigDecimal]
      paymentDate <- dateGenerator
      benefitsPaidEarly <- Gen.alphaStr
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
        )
      )

      def freeTxtOrSchemeOrRecipientName = paymentNature match {
        case "benefitInKind" => benefitInKindDesc
        case "errorCalcTaxFreeLumpSums" => errorDesc
        case "transferToNonRegPensionScheme" => schemeName
        case "benefitsPaidEarly" => benefitsPaidEarly
        case _ => ""
      }

      val unAuthorisedPmtType2: String =
        paymentNature match {
          case "transferToNonRegPensionScheme" => whoWasTransferMadeToMap(whoWasTransferMadeTo)
          case "refundOfContributions" => refundOfContributionsMap(refundOfContributions)
          case "overpaymentOrWriteOff" => overpaymentOrWriteOffMap(overpaymentOrWriteOff)
          case _ => ""
        }


      def unauthorsedPaymentDetails: JsObject = Json.obj(
        "unAuthorisedPmtType1" -> paymentNatureTypesMember(paymentNature),
        "freeTxtOrSchemeOrRecipientName" -> freeTxtOrSchemeOrRecipientName,
        "unAuthorisedPmtType2" -> unAuthorisedPmtType2,
        "valueOfUnauthorisedPayment" -> paymentVal,
        "dateOfUnauthorisedPayment" -> paymentDate
      ) ++ (
        if (paymentNature == "transferToNonRegPensionScheme") {
          Json.obj("pstrOrReference" -> pstrOrReference(paymentNature, schemeRef))
        } else {
          Json.obj()
        }
        )

      val expectedJson = Json.obj(
        "individualMemberDetails" -> Json.obj(
          "firstName" -> firstName,
          "lastName" -> lastName,
          "nino" -> nino,
          "signedMandate" -> signedMandate,
          "pmtMoreThan25PerFundValue" -> unAuthorisedPayment,
          "schemePayingSurcharge" -> unAuthPaySurcharge
        ),
        "unAuthorisedPaymentDetails" ->
          unauthorsedPaymentDetails
      )
      Tuple2(ua, expectedJson)
    }
  }

  //scalastyle:off
  def generateRandomPayloadAPI1827: Gen[Tuple2[JsObject, JsObject]] = {
    val whoReceivedUnauthorisedPaymentMember = "member"
    val whoReceivedUnauthorisedPaymentEmployer = "employer"
    val whoReceivedUnauthorisedPaymentMap: Map[String, String] = Map(
      whoReceivedUnauthorisedPaymentMember -> "Individual",
      whoReceivedUnauthorisedPaymentEmployer -> "Employer",
    )
    Gen.oneOf(whoReceivedUnauthorisedPaymentMember, whoReceivedUnauthorisedPaymentMember /*whoReceivedUnauthorisedPaymentEmployer*/)
      .flatMap { whoReceivedUnauthorisedPayment =>
        (whoReceivedUnauthorisedPayment match {
          case `whoReceivedUnauthorisedPaymentMember` => generateMember
          case _ => generateMember // TODO: Change to generateEmployer
        }).map { case (generatedUA, generatedExpectedResult) =>
          val fullUA = Json.obj(
            "membersOrEmployers" ->
              Json.arr(
                generatedUA ++ Json.obj("whoReceivedUnauthPayment" -> whoReceivedUnauthorisedPayment)
              )
          )
          val fullExpectedResult = Json.obj(
            "event1Details" -> Json.obj(
              "event1Details" -> Json.arr(
                generatedExpectedResult ++ Json.obj("memberType" -> whoReceivedUnauthorisedPaymentMap(whoReceivedUnauthorisedPayment))
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
