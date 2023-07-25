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
import utils.{GeneratorAPI1831, JsonFileReader}

class API1537Spec extends AnyFreeSpec
  with Matchers with MockitoSugar with JsonFileReader with ScalaCheckPropertyChecks with GeneratorAPI1831 {
  "Reads" - {

    "transform a valid payload from API 1533 correctly" in {
      val payload = Json.parse("""  [
                      |    {
                      |      "reportFormBundleNumber": "123456785015",
                      |      "reportVersion": 1,
                      |      "reportStatus": "SubmittedAndInProgress",
                      |      "compilationOrSubmissionDate": "2021-04-01T09:30:47Z",
                      |      "reportSubmitterDetails": {
                      |        "reportSubmittedBy": "PSP",
                      |        "orgOrPartnershipDetails": {
                      |          "orgOrPartnershipName": "ABC Limited"
                      |        }
                      |      },
                      |      "psaDetails": {
                      |        "psaOrgOrPartnershipDetails": {
                      |          "orgOrPartnershipName": "XYZ Limited"
                      |        }
                      |      }
                      |    }
                      |  ]""".stripMargin)

      val expected = Json.arr(Json.obj(
        "versionInfo" -> Json.obj(
        "version"-> 1,
          "status" -> "submitted"
      ),
        "submitterName" ->  "ABC Limited"
      ))

      val result = payload.validate(API1537.reads).asOpt

      result mustBe Some(expected)
    }
  }

}
