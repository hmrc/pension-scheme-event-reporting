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

  import utils.GeneratorAPI1832._

  def generateUserAnswersAndPOSTBodyByEvent(eventType: EventType): Gen[(JsObject, JsObject)] = {
    eventType match {
      case Event2 => generateUAFromETMPDataForEvent2
      case Event3 => ???
      case Event4 => ???
      case Event5 => ???
      case Event6 => ???
      case Event7 => ???
      case Event8 => ???
      case Event8A => ???
      case _ => generateUAFromETMPDataForEvent22And23(eventType)
    }
  }

  def generateUAFromETMPDataForEvent22And23(eventType: EventType): Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(eventType) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "event" -> Json.obj(
              "eventType" -> s"Event${Event2.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "paymentDetails" -> Json.obj(
                "monetaryAmount" -> map("pensionAmt"),
                "taxYearEndingDate" -> s"${map("taxYearEndDate")}-04-05"
              )
            )
          )
          )
        )
        )

      val userAnswers = Json.obj(
        s"event${eventType.toString}" -> Json.obj(
          "members" -> Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "chooseTaxYear" -> (map("taxYearEndDate").toInt - 1).toString,
              "totalPensionAmounts" -> map("pensionAmt")
            )
          )
        )
      )


      Tuple2(etmpPayload, userAnswers)
    }
  }

  def generateUAFromETMPDataForEvent2: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
        val etmpPayload = etmpData(Event2) ++
          Json.obj("eventDetails" -> Json.arr(
            Json.obj("memberDetail" -> Json.obj(
          "event" -> Json.obj(
            "eventType" -> s"Event${Event2.toString}",
                    "individualDetails" -> Json.obj(
                      "firstName" -> map("firstName"),
                      "lastName" -> map("lastName"),
                      "nino" -> map("nino")
                    ),
                    "personReceivedThePayment" -> Json.obj(
                      "firstName" -> map("firstName"),
                      "lastName" -> map("lastName"),
                      "nino" -> map("nino")
                    ),
                    "paymentDetails" -> Json.obj(
              "amountPaid" -> map("pensionAmt"),
                      "eventDate" -> s"${map("taxYearEndDate")}-04-05"
                    )
                  )
                )
              )
            )
          )
      val userAnswers = Json.obj(
        s"event${Event2.toString}" -> Json.obj(
          "members" -> Json.arr(
            Json.obj(
              "deceasedMembersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "beneficiaryDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "datePaid" -> (map("taxYearEndDate").toInt - 1).toString,
              "amountPaid" -> map("pensionAmt")
            )
          )
        )
      )
      Tuple2(etmpPayload, userAnswers)
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


//noinspection ScalaStyle
object GeneratorAPI1832 {

  private def etmpData(eventType: EventType): JsObject = Json.obj(
    "processingDate" -> "2023-12-15T12:30:46Z",
    "schemeDetails" -> Json.obj(
      "pSTR" -> "87219363YN",
      "schemeName" -> "Abc Ltd"
    ),
    "eventReportDetails" -> Json.obj(
      "reportStartDate" -> "2021-04-06",
      "reportEndDate" -> "2022-04-05",
      "reportStatus" -> "Compiled",
      "reportVersionNumber" -> "001",
      "reportSubmittedDateAndTime" -> "2023-12-13T12:12:12Z",
      "eventType" -> s"Event${eventType.toString}"
    ),
    "eventDetails" -> Json.arr(
      Json.obj(
        "memberDetail" -> Json.obj(
          "memberStatus" -> "New",
          "event" -> Json.obj(

          )
        )
      )
    )
  )

  private def randomValues(): Gen[Map[String, String]] = {
    for {
      firstName <- Gen.oneOf(Seq("Alice", "Bob", "Charlie"))
      lastName <- Gen.oneOf(Seq("Xavier", "Yilmaz", "Zimmer"))
      nino <- Gen.oneOf(Seq("AB123456C", "CD123456E"))
      pensionAmt <- Gen.chooseNum(1, 1000)
      taxYearEndDate <- Gen.oneOf(2020, 2021, 2022)
    } yield {
      Map(
        "firstName" -> firstName,
        "lastName" -> lastName,
        "nino" -> nino,
        "pensionAmt" -> pensionAmt.toString,
        "taxYearEndDate" -> taxYearEndDate.toString,
      )
    }
  }
}
