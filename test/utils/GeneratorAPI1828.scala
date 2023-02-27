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

import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}

// TODO: Consider relocation of code to different package.
trait GeneratorAPI1828 extends Matchers with OptionValues with ResponseGenerators {
  def generateUserAnswersAndPOSTBody: Gen[(JsObject, JsObject)] = {
    for {
      date <- dateGenerator
      pstr <- pstrGen
      psaOrPsp <- psaOrPspGen
      psaOrPspId <- psaOrPspIdGen
    } yield {
      val fullUA = Json.obj(
        "pSTR" -> pstr,
        "reportStartDate" -> date,
        "reportEndDate" -> date.plusDays(1),
        "submittedBy" -> psaOrPsp,
        "submittedID" -> psaOrPspId
      )
      val fullExpectedResult = Json.obj(
        "declarationDetails" -> Json.obj(
          "erDetails" -> Json.obj(
            "pSTR" -> "Hello"
          )
        )
      )
      Tuple2(fullUA, fullExpectedResult)
    }
  }
}

/*
{
  "declarationDetails": {
    "erDetails": {
      "pSTR": "27176941JF",
      "reportStartDate": "2020-04-06",
      "reportEndDate": "2021-04-05"
    },
    "erDeclarationDetails": {
      "submittedBy": "PSP",
      "submittedID": "20345678"
    },
    "pspDeclaration": {
      "authorisedPSAID": "A2345678",
      "pspDeclaration1": "Selected",
      "pspDeclaration2": "Selected"
    }
  }
}
 */