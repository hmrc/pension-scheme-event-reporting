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

package utils

import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsValue, Json}


trait GeneratorAPI1831 extends Matchers with OptionValues with ResponseGenerators {
  //scalastyle:off method.length
  def generateUserAnswersAndPOSTBody: Gen[(JsValue, JsValue)] = {
    for {
      pstr <- pstrGen
      reportStartDate <- reportStartDateGenerator
      reportEndDate = reportStartDate.plusYears(1).minusDays(1)
      schemeMasterTrustDate = reportStartDate.plusMonths(4)
      startOrCease <- schemeMasterTrustStartOrCeaseDate
      psaOrPsp <- psaOrPspGen
      psaOrPspIdGen =  psaOrPsp match {
        case "PSA" => psaIdGen
        case "PSP" => pspIdGen
        case _ => throw new RuntimeException("Generation of ID value failed")
      }
      id <- psaOrPspIdGen
      psaOrPspDeclarationETMP = if (psaOrPsp == "PSA") {
        """ "psaDeclaration": {"psaDeclaration1": "Selected", "psaDeclaration2": "Selected"} """
      } else {
        """ "pspDeclaration": {"authorisedPSAID": "A4045157", "pspDeclaration1": "Selected", "pspDeclaration2": "Selected"} """
      }
      psaOrPspDeclarationUA = if (psaOrPsp == "PSA") {
        """ "psaDeclaration1": "Selected", "psaDeclaration2": "Selected" """
      } else {
        """ "authorisedPSAID": "A4045157", "pspDeclaration1": "Selected", "pspDeclaration2": "Selected" """
      }
    } yield {
      val payload: JsValue = Json.parse(
       s"""
          |{
          |  "processingDate": "2023-12-15T12:30:46Z",
          |  "schemeDetails": {
          |    "pSTR": "$pstr",
          |    "schemeName": "Abc Ltd"
          |  },
          |  "er20aDetails": {
          |    "reportStartDate": "$reportStartDate",
          |    "reportEndDate": "$reportEndDate",
          |    "reportVersionNumber": "001",
          |    "reportSubmittedDateAndTime": "2023-12-13T12:12:12Z"
          |  },
          |  "schemeMasterTrustDetails": {
          |    "$startOrCease": "$schemeMasterTrustDate"
          |  },
          |  "erDeclarationDetails": {
          |    "submittedBy": "$psaOrPsp",
          |    "submittedID": "$id",
          |    "submittedName": "ABCDEFGHIJKLMNOPQRSTUV",
          |    $psaOrPspDeclarationETMP
          |  }
          |}
          |""".stripMargin
      )

      val expected = Json.parse(
       s"""
          |{
          |  "pstr": "$pstr",
          |  "reportStartDate": "$reportStartDate",
          |  "reportEndDate": "$reportEndDate",
          |  "schemeMasterTrust${startOrCease.capitalize}": "$schemeMasterTrustDate",
          |  "submittedBy": "$psaOrPsp",
          |  "submittedID": "$id",
          |  $psaOrPspDeclarationUA
          |}
          |""".stripMargin)

      Tuple2(payload, expected)
    }
  }
}
