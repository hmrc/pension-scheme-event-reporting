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

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1832, GeneratorAPI1834, JsonFileReader}

class EventOneReportSpec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader
  with GeneratorAPI1834 with GeneratorAPI1832 with ScalaCheckPropertyChecks {

  import EventOneReportSpec._

  "Reads" - {
    "transform a valid individual payload from API 1833 correctly" in {

      (individualPayload, expectedIndividualResponse) match {
        case (payload: JsObject, expectedResponse: JsObject) =>
          val result = payload.validate(EventOneReport.rds1833Api).asOpt
          result mustBe Some(expectedResponse)
      }
    }

    "transform a valid employer payload from API 1833 correctly" in {

      (employerPayload, expectedEmployerResponse) match {
        case (payload: JsObject, expectedResponse: JsObject) =>
          val result = payload.validate(EventOneReport.rds1833Api).asOpt
          result mustBe Some(expectedResponse)
      }
    }
  }
}

object EventOneReportSpec {

  val individualPayload: JsValue = Json.parse(
    """
      |{
      |  "processingDate": "2023-12-15T12:30:46Z",
      |  "schemeDetails": {
      |    "pSTR": "87219363YN",
      |    "schemeName": "Abc Ltd"
      |  },
      |  "eventReportDetails": {
      |    "reportFormBundleNumber": "123456789012",
      |    "reportStartDate": "2021-04-06",
      |    "reportEndDate": "2022-04-05",
      |    "reportStatus": "Compiled",
      |    "reportVersionNumber": "001",
      |    "reportSubmittedDateAndTime": "2023-12-13T12:12:12Z"
      |  },
      |  "event1Details": [
      |    {
      |      "memberStatus": "New",
      |      "memberType": "Individual",
      |      "individualMemberDetails": {
      |        "title": "Mr",
      |        "firstName": "John",
      |        "middleName": "Mac",
      |        "lastName": "Smith",
      |        "nino": "AA999999A",
      |        "signedMandate": "Yes",
      |        "pmtMoreThan25PerFundValue": "No",
      |        "schemePayingSurcharge": "Yes"
      |      },
      |      "unAuthorisedPaymentDetails": {
      |        "unAuthorisedPmtType1": "Transfer to non-registered pensions scheme",
      |        "unAuthorisedPmtType2": "Transfer to an Employer Financed retirement Benefit scheme (EFRB)",
      |        "freeTxtOrSchemeOrRecipientName": "ABCDEFGHIJKLMNOPQRSTUV",
      |        "pstrOrReference": "20034565RX",
      |        "dateOfUnauthorisedPayment": "2020-06-30",
      |        "valueOfUnauthorisedPayment": 723
      |      }
      |    }
      |  ]
      |}
      |""".stripMargin)

  val expectedIndividualResponse: JsValue = Json.parse(
    """
      |{
      |  "event1": {
      |    "membersOrEmployers": [
      |      {
      |        "schemeDetails": {
      |          "reference": "20034565RX",
      |          "schemeName": "ABCDEFGHIJKLMNOPQRSTUV"
      |        },
      |        "doYouHoldSignedMandate": true,
      |        "schemeUnAuthPaySurchargeMember": true,
      |        "membersDetails": {
      |          "lastName": "Smith",
      |          "firstName": "John",
      |          "nino": "Smith"
      |        },
      |        "whoWasTheTransferMade": "anEmployerFinanced",
      |        "whoReceivedUnauthPayment": "member",
      |        "paymentValueAndDate": {
      |          "paymentValue": 723,
      |          "paymentDate": "2020-06-30"
      |        },
      |        "paymentNatureMember": "transferToNonRegPensionScheme",
      |        "valueOfUnauthorisedPayment": false
      |      }
      |    ]
      |  }
      |}
      |""".stripMargin)

  val employerPayload: JsValue = Json.parse(
    """
      |{
      |  "processingDate": "2023-12-15T12:30:46Z",
      |  "schemeDetails": {
      |    "pSTR": "87219363YN",
      |    "schemeName": "Abc Ltd"
      |  },
      |  "eventReportDetails": {
      |    "reportFormBundleNumber": "123456789012",
      |    "reportStartDate": "2021-04-06",
      |    "reportEndDate": "2022-04-05",
      |    "reportStatus": "Compiled",
      |    "reportVersionNumber": "001",
      |    "reportSubmittedDateAndTime": "2023-12-13T12:12:12Z"
      |  },
      |  "event1Details": [
      |    {
      |      "memberStatus": "New",
      |      "memberType": "Employer",
      |      "employerMemberDetails": {
      |        "compOrOrgName": "The Biz Ltd",
      |        "crnNumber": "12345679",
      |        "addressDetails": {
      |         "addressLine1": "123 Fun Lane",
      |         "addressLine2": "Cool Crescent",
      |         "addressLine3": "Slough",
      |         "addressLine4": "Berkshire",
      |         "postCode": "ZZ11 1ZZ",
      |         "countryCode": "GB"
      |        }
      |      },
      |      "unAuthorisedPaymentDetails": {
      |        "unAuthorisedPmtType1": "Tangible moveable property held directly or indirectly by an investment-regulated pension scheme",
      |        "unAuthorisedPmtType2": "Transfer to an Employer Financed retirement Benefit scheme (EFRB)",
      |        "freeTxtOrSchemeOrRecipientName": "Sample description",
      |        "dateOfUnauthorisedPayment": "2020-06-30",
      |        "valueOfUnauthorisedPayment": 723
      |      }
      |    }
      |  ]
      |}
      |""".stripMargin)

  val expectedEmployerResponse: JsValue = Json.parse(
    """
      |{
      |  "event1": {
      |    "membersOrEmployers": [
      |      {
      |        "employerTangibleMoveableProperty": "Sample description",
      |        "employerAddress": {
      |          "addressLine1": "123 Fun Lane",
      |          "addressLine2": "Cool Crescent",
      |          "addressLine3": "Slough",
      |          "addressLine4": "Berkshire",
      |          "postCode": "ZZ11 1ZZ",
      |          "countryCode": "GB"
      |        },
      |        "companyDetails": {
      |          "companyNumber": "12345679",
      |          "companyName": "The Biz Ltd"
      |        },
      |        "whoWasTheTransferMade": "anEmployerFinanced",
      |        "whoReceivedUnauthPayment": "employer",
      |        "paymentValueAndDate": {
      |          "paymentValue": 723,
      |          "paymentDate": "2020-06-30"
      |        },
      |        "paymentNatureEmployer": "tangibleMoveableProperty"
      |      }
      |    ]
      |  }
      |}
      |""".stripMargin)
}
