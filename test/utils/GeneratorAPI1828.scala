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
      startDate <- dateGenerator
      pstr <- pstrGen
      psaOrPsp <- psaOrPspGen
      psaOrPspId <- psaOrPspIdGen
    } yield {
      val endDate = startDate.plusDays(1)
      val psa = "PSA"
      val fullUA = Json.obj(
        "pstr" -> pstr,
        "reportStartDate" -> startDate,
        "reportEndDate" -> startDate.plusDays(1),
        "submittedBy" -> psa, // TODO: amend to use psaOrPspGen when that functionality is required.
        "submittedID" -> psaOrPspId
      )
      val fullExpectedResult = Json.obj(
        "declarationDetails" -> Json.obj(
          "erDetails" -> Json.obj(
            "pSTR" -> pstr,
            "reportStartDate" -> startDate,
            "reportEndDate" -> endDate,
          ),
          "erDeclarationDetails" -> Json.obj(
            "submittedBy" -> psa,
            "submittedID" -> psaOrPspId
          ),
          "psaDeclaration" -> Json.obj( // TODO: amend to be psaDec or pspDec when that functionality is required.
            "psaDeclaration1" -> "Selected",
            "psaDeclaration2" -> "Selected"
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