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

import models.enumeration.EventType
import models.enumeration.EventType.{Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event8A}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, JsValue, Json}

trait GeneratorAPI1832 extends Matchers with OptionValues with ResponseGenerators {

  def generateUserAnswersAndPOSTBodyByEvent(eventType: EventType): Gen[(JsObject, JsObject)] = {
    eventType match {
      case Event2 => ???
      case Event3 => ???
      case Event4 => ???
      case Event5 => ???
      case Event6 => ???
      case Event7 => ???
      case Event8 => ???
      case Event8A => ???
      case _ => generateGET1832UserAnswersFromETMP(eventType)
    }
  }

  def generateGET1832UserAnswersFromETMP(eventType: EventType): Gen[(JsObject, JsObject)] = {
    for {
      firstName <- Gen.alphaStr
      lastName <- Gen.alphaStr
      nino <- Gen.alphaStr
      pensionAmt <- arbitrary[BigDecimal]
      taxYearEndDate <- Gen.oneOf(2020, 2021, 2022)
    } yield {
      val fullPayload = Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "event" -> Json.obj(
              "eventType" -> s"Event${eventType.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> firstName,
                "lastName" -> lastName,
                "nino" -> nino),
              "paymentDetails" -> Json.obj(
                "monetaryAmount" -> pensionAmt,
                "taxYearEndingDate" -> s"$taxYearEndDate-04-05"
              )
            )
          ))
        ))
      val fullExpectedResult = Json.obj(
        s"event${eventType.toString}" -> Json.obj(
          "members" -> Json.arr(Json.obj(
            "membersDetails" -> Json.obj(
              "firstName" -> firstName,
              "lastName" -> lastName,
              "nino" -> nino
            ),
            "chooseTaxYear" -> (taxYearEndDate - 1).toString,
            "totalPensionAmounts" -> pensionAmt
          )
          )
        )
      )
      Tuple2(fullPayload, fullExpectedResult)
    }
  }


  def generatedPayload(eventType: EventType): JsValue = {
    Json.parse(
      s"""
         |{
         |  "processingDate": "2023-12-15T12:30:46Z",
         |  "schemeDetails": {
         |    "pSTR": "87219363YN",
         |    "schemeName": "Abc Ltd"
         |  },
         |  "eventReportDetails": {
         |    "reportStartDate": "2021-04-06",
         |    "reportEndDate": "2022-04-05",
         |    "reportStatus": "Compiled",
         |    "reportVersionNumber": "001",
         |    "reportSubmittedDateAndTime": "2023-12-13T12:12:12Z",
         |    "eventType": "Event${eventType.toString}"
         |  },
         |  "eventDetails": [
         |    {
         |      "memberDetail": {
         |        "memberStatus": "New",
         |        "event": {
         |          "eventType": "Event${eventType.toString}",
         |          "individualDetails": {
         |            "title": "Mr",
         |            "firstName": "John",
         |            "middleName": "A",
         |            "lastName": "Smith",
         |            "nino": "AA345678B"
         |          },
         |          "paymentDetails": {
         |            "monetaryAmount": 123.99,
         |            "taxYearEndingDate": "2021-05-30"
         |          }
         |        }
         |      }
         |    }
         |  ]
         |}
         |""".stripMargin)
  }


  def generateGET1832ResponseAndUserAnswers: Gen[(JsValue, EventType)] = {
    for {
      chosenEventType <- Gen.oneOf[EventType](Seq(EventType.Event22, EventType.Event23))
    } yield {
      Tuple2(generatedPayload(chosenEventType), chosenEventType)
    }
  }
}
