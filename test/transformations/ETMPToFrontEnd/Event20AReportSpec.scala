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
import utils.JsonFileReader

class Event20AReportSpec extends AnyFreeSpec with Matchers with MockitoSugar with JsonFileReader with ScalaCheckPropertyChecks {

  import Event20AReportSpec._

  "Reads" - {
    "transform a valid payload from API 1831 correctly" in {

      (payload, response) match {
        case (payload: JsObject, expectedResponse: JsObject) =>
          val result = payload.validate(Event20AReport.rds1831Api).asOpt
          result mustBe Some(expectedResponse)
      }
    }
  }
}

object Event20AReportSpec {
  val payload: JsValue = Json.parse(
    """
      |{
      |  "processingDate": "2023-12-15T12:30:46Z",
      |  "schemeDetails": {
      |    "pSTR": "87219363YN",
      |    "schemeName": "Abc Ltd"
      |  },
      |  "er20aDetails": {
      |    "reportStartDate": "2021-04-06",
      |    "reportEndDate": "2022-04-05",
      |    "reportVersionNumber": "001",
      |    "reportSubmittedDateAndTime": "2023-12-13T12:12:12Z"
      |  },
      |  "schemeMasterTrustDetails": {
      |    "startDate": "2021-06-08"
      |  },
      |  "erDeclarationDetails": {
      |    "submittedBy": "PSP",
      |    "submittedID": "20000001",
      |    "submittedName": "ABCDEFGHIJKLMNOPQRSTUV",
      |    "pspDeclaration": {
      |      "authorisedPSAID": "A4045157",
      |      "pspDeclaration1": "Selected",
      |      "pspDeclaration2": "Selected"
      |    }
      |  }
      |}
      |""".stripMargin
  )

  val response: JsValue = Json.parse(
    """
      |{
      |  "pspDeclaration2": "Selected",
      |  "submittedBy": "PSP",
      |  "pspDeclaration1": "Selected",
      |  "schemeMasterTrustStartDate": "2021-06-08",
      |  "reportStartDate": "2021-04-06",
      |  "submittedID": "20000001",
      |  "pstr": "87219363YN",
      |  "authorisedPSAID": "A4045157",
      |  "reportEndDate": "2022-04-05"
      |}
      |""".stripMargin
  )
}


