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

trait GeneratorAPI1834 extends Matchers with OptionValues with ResponseGenerators {
  def generateGET1834ResponseAndUserAnswers: Gen[Tuple2[JsObject, Seq[String]]] = {
    val sortEventTypes: (String, String) => Boolean = (a, b) => {
      def toNum(str:String) = {
        val splitString = str.split("A")
        if(splitString.length == 1) splitString(0).toDouble
        else splitString(0).toDouble
      }
      val aNum = toNum(a)
      val bNum = toNum(b)
      (aNum, bNum) match {
        case (0, _) => false
        case (_, 0) => true
        case (a, b) if a < b => true
        case _ => false
      }
    }
    val version = "001"
    for {
      chosenEventTypesWithSeq <- Gen.someOf[String](Seq("10", "13", "19", "20"))
      chosenEventTypesWithoutSeq <- Gen.someOf[String](Seq("11", "12", "14", "0"))
      chosenMemberEventTypesSeq <- Gen.someOf[String](Seq("2", "3", "4", "5", "6", "7", "8", "8A", "22", "23", "24"))
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

      val membersPayloadSeq = chosenMemberEventTypesSeq.foldLeft(Json.obj()) { (acc, s) =>
        acc ++ Json.obj(s"event$s" -> Json.obj("recordVersion" -> version))
      }

      val generatedPayload = Json.obj(
        "eventDetails" -> (payloadWithSeq ++ payloadWithoutSeq),
        "memberEventsSummary" -> membersPayloadSeq
      )

      val expectedEventTypes = (chosenEventTypesWithSeq ++ chosenEventTypesWithoutSeq ++ chosenMemberEventTypesSeq)
        .sortWith(sortEventTypes).toSeq

      Tuple2(generatedPayload, expectedEventTypes)
    }
  }


}
