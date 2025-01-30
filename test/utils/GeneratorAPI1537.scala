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
import play.api.libs.json.{JsArray, JsObject, Json}

import java.time.LocalDate

//noinspection ScalaStyle
trait GeneratorAPI1537 extends ResponseGenerators {

  def generateGetVersionJson: Gen[(JsArray, JsArray)] = {
    for {
      reportStatus <- Gen.oneOf(Seq("SubmittedAndInProgress", "SubmittedAndSuccessfullyProcessed", "Compiled"))
      version <- Gen.oneOf(1, 2, 3)
      orgOrInd <- Gen.oneOf("organisationOrPartnershipDetails", "individualDetails")
    } yield {

      val expectedReportStatus = reportStatus match {
        case "SubmittedAndInProgress" => "submitted"
        case "SubmittedAndSuccessfullyProcessed" => "submitted"
        case _ => "compiled"
      }

      val expectedSubmitterName = orgOrInd match {
        case "organisationOrPartnershipDetails" => "ABC Limited"
        case _ => "Simon Walker"
      }

      def orgOrIndividual: (String, JsObject) = orgOrInd match {
        case "organisationOrPartnershipDetails" =>
          "organisationOrPartnershipDetails" -> Json.obj(
            "organisationOrPartnershipName" -> "ABC Limited"
          )
        case _ =>
          "individualDetails" -> Json.obj(
            "firstName" -> "Simon",
            "lastName" -> "Walker"
          )
      }

      val payload = Json.arr( Json.obj(
        "reportFormBundleNumber" -> "123456785015",
        "reportVersion" -> version,
        "reportStatus" -> reportStatus,
        "compilationOrSubmissionDate" -> "2021-04-01T09:30:47Z",
        "reportSubmitterDetails" -> Json.obj(
          "reportSubmittedBy" -> "PSP",
          orgOrIndividual._1 -> orgOrIndividual._2
        ),
        "psaDetails" -> Json.obj(
          "psaOrgOrPartnershipDetails" -> Json.obj(
            "orgOrPartnershipName" -> "XYZ Limited"
          )
        )
      ))
      val expected = Json.arr(Json.obj(
        "versionDetails" -> Json.obj(
          "version" -> version,
          "status" -> expectedReportStatus
        ),
        "submitterName" -> expectedSubmitterName,
        "submittedDate" -> LocalDate.of(2021,4, 1)
      ))

      Tuple2(payload, expected)
    }
  }

  def generateGetVersionMissingSubmitterJson: Gen[(JsArray, JsArray)] = {
    for {
      reportStatus <- Gen.oneOf(Seq("SubmittedAndInProgress", "SubmittedAndSuccessfullyProcessed", "Compiled"))
      version <- Gen.oneOf(1, 2, 3)
    } yield {

      val expectedReportStatus = reportStatus match {
        case "SubmittedAndInProgress" => "submitted"
        case "SubmittedAndSuccessfullyProcessed" => "submitted"
        case _ => "compiled"
      }

      val payload = Json.arr(Json.obj(
        "reportFormBundleNumber" -> "123456785015",
        "reportVersion" -> version,
        "reportStatus" -> reportStatus,
        "compilationOrSubmissionDate" -> "2021-04-01T09:30:47Z",
        "psaDetails" -> Json.obj(
          "psaOrgOrPartnershipDetails" -> Json.obj(
            "orgOrPartnershipName" -> "XYZ Limited"
          )
        )
      ))
      val expected = Json.arr(Json.obj(
        "versionDetails" -> Json.obj(
          "version" -> version,
          "status" -> expectedReportStatus
        ),

        "submittedDate" -> LocalDate.of(2021, 4, 1)
      ))

      Tuple2(payload, expected)
    }
  }





}