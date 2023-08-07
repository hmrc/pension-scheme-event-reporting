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

// scalastyle:off
trait GeneratorAPI1828 extends Matchers with OptionValues with ResponseGenerators {
  def generateUserAnswersAndPOSTBody: Gen[(JsObject, JsObject)] = {
    for {
      startDate <- dateGenerator
      pstr <- pstrGen
      psaOrPsp <- psaOrPspGen
      psaId <- psaIdGen
      pspId <- pspIdGen
    } yield {
      val endDate = startDate.plusDays(1)
      val selected = "Selected"
      def psaOrPspId(psaOrPsp: String): String = {
        psaOrPsp match {
          case "PSA" => psaId
          case "PSP" => pspId
        }
      }
      def psaOrPspDeclaration(psaOrPsp: String): (String, Json.JsValueWrapper) = {
        psaOrPsp match {
          case "PSA" => "psaDeclaration" -> Json.obj(
            "psaDeclaration1" -> selected,
            "psaDeclaration2" -> selected
          )
          case "PSP" => "pspDeclaration" -> Json.obj(
            "authorisedPSAID" -> psaId,
            "pspDeclaration1" -> selected,
            "pspDeclaration2" -> selected
          )
        }
      }

      val psaOrPspJson = psaOrPspDeclaration(psaOrPsp)

      def fullUA(psaOrPsp: String) = psaOrPsp match {
        case "PSA" => Json.obj(
          "pstr" -> pstr,
          "reportStartDate" -> startDate,
          "reportEndDate" -> startDate.plusDays(1),
          "submittedBy" -> psaOrPsp,
          "submittedID" -> psaOrPspId(psaOrPsp),
          "psaDeclaration1" -> selected,
          "psaDeclaration2" -> selected
        )
        case "PSP" => Json.obj(
          "pstr" -> pstr,
          "reportStartDate" -> startDate,
          "reportEndDate" -> startDate.plusDays(1),
          "submittedBy" -> psaOrPsp,
          "submittedID" -> psaOrPspId(psaOrPsp),
          "authorisedPSAID" -> psaId,
          "pspDeclaration1" -> selected,
          "pspDeclaration2" -> selected
        )
      }

      val fullExpectedResult = Json.obj(
        "declarationDetails" -> Json.obj(
          "erDetails" -> Json.obj(
            "pSTR" -> pstr,
            "reportStartDate" -> startDate,
            "reportEndDate" -> endDate,
          ),
          "erDeclarationDetails" -> Json.obj(
            "submittedBy" -> psaOrPsp,
            "submittedID" -> psaOrPspId(psaOrPsp)
          ),
          psaOrPspJson
        )
      )
      Tuple2(fullUA(psaOrPsp), fullExpectedResult)
    }
  }
}
