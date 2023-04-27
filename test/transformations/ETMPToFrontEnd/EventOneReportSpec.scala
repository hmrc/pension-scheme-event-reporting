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

  "Reads" - {
    "transform a randomly generated valid payload from API 1833 correctly" in {

      // TODO: Create one valid payload for a member and an employer. Alternatively: a full generator.
      val payload = Json.parse("""{
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
                                 |}""".stripMargin)

      // TODO: create valid response for member and employer. Alternatively: a full generator. Tests will fail if you don't change line below.
      val expectedResponse = Json.obj("hello" -> "world")

      (payload, expectedResponse) match {
        case (payload: JsObject, expectedResponse: JsObject) =>
          val result = payload.validate(EventOneReport.rds1833Api).asOpt
          result mustBe Some(expectedResponse)
      }
    }
  }
}
