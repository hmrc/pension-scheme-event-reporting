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
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, JsValue, Json}


//noinspection ScalaStyle
trait GeneratorAPI1832 extends Matchers with OptionValues with ResponseGenerators {

  import utils.GeneratorAPI1832._

  def generateUserAnswersAndPOSTBodyByEvent(eventType: EventType): Gen[(JsObject, JsObject)] = {
    eventType match {
      case Event2 => generateUAFromETMPDataForEvent2
      case Event3 => generateUAFromETMPDataForEvent3
      case Event4 | Event5 => generateUAFromETMPDataForEvent4And5(eventType)
      case Event6 => generateUAFromETMPDataForEvent6
      case Event7 => generateUAFromETMPDataForEvent7
      case Event8 => generateUAFromETMPDataForEvent8
      case Event8A => generateUAFromETMPDataForEvent8A
      case _ => generateUAFromETMPDataForEvent22And23(eventType)
    }
  }

  def generateUAFromETMPDataForEvent2: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(Event2) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "memberStatus" -> "New",
            "event" -> Json.obj(
              "eventType" -> s"Event${Event2.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> map("deceasedFirstName"),
                "lastName" -> map("deceasedLastName"),
                "nino" -> map("deceasedNino"),
              ),
              "personReceivedThePayment" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "paymentDetails" -> Json.obj(
                "amountPaid" -> map("pensionAmt"),
                "eventDate" -> s"${map("taxYearEndDate")}-04-05",
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
                "firstName" -> map("deceasedFirstName"),
                "lastName" -> map("deceasedLastName"),
                "nino" -> map("deceasedNino")
              ),
              "beneficiaryDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "datePaid" -> s"${map("taxYearEndDate")}-04-05",
              "amountPaid" -> map("pensionAmt")
            )
          )
        )
      )
      Tuple2(etmpPayload, userAnswers)
    }
  }

  def generateUAFromETMPDataForEvent3: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {

      val etmpPayload = etmpData(Event3) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "memberStatus" -> "New",
            "event" -> Json.obj(
              "eventType" -> s"Event${Event3.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "paymentDetails" -> Json.obj(
                "reasonBenefitTaken" -> map("reasonBenefitTakenEvent3"),
                "amountBenefit" -> map("pensionAmt"),
                "eventDate" -> s"${map("taxYearEndDate")}-04-05",
                "freeText" -> freeTextEvent3(map("reasonBenefitTakenEvent3")
                )
            )
          )
          )
        )
        ))
      val userAnswers = Json.obj(
        s"event${Event3.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")),
              "benefitType" -> Json.obj(
                "reasonBenefitTaken" -> map("reasonBenefitTakenEvent3"),
                "freeText" -> freeTextEvent3(map("reasonBenefitTakenEvent3"))
              ),
              "paymentDetails" -> Json.obj(
                "amountBenefit" -> map("pensionAmt"),
                "eventDate" -> s"${map("taxYearEndDate")}-04-05"
              ),
            )
          )
        )
      )
      Tuple2(etmpPayload, userAnswers)
    }
  }

  def generateUAFromETMPDataForEvent4And5(eventType: EventType): Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(eventType) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "event" -> Json.obj(
              "eventType" -> s"Event${eventType.toString}",
              "individualDetails" -> Json.obj(
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
        s"event${eventType.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")),
              "paymentDetails" -> Json.obj(
                "amountPaid" -> map("pensionAmt"),
                "datePaid" -> s"${map("taxYearEndDate")}-04-05"
              ),
            )
          )
        )
      )
      Tuple2(etmpPayload, userAnswers)
    }
  }

  def generateUAFromETMPDataForEvent6: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(Event6) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "event" -> Json.obj(
              "eventType" -> s"Event${Event6.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "paymentDetails" -> Json.obj(
                "amountCrystalised" -> map("amountCrystallised"),
                "typeOfProtection" -> map("typeOfProtectionEvent6"),
                "eventDate" -> s"${map("taxYearEndDate")}-04-05",
                "freeText" -> map("inputProtectionType")
              )
            )
          )
          )
        )
        )
      val userAnswers = Json.obj(
        s"event${Event6.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")),
              "typeOfProtection" -> map("typeOfProtectionEvent6"),
              "inputProtectionType" -> map("inputProtectionType"),
              "AmountCrystallisedAndDate" -> Json.obj(
                "amountCrystallised" -> map("amountCrystallised"),
                "crystallisedDate" -> s"${map("taxYearEndDate")}-04-05",
              ),
            )
          )
        )
      )
      Tuple2(etmpPayload, userAnswers)
    }
  }

  def generateUAFromETMPDataForEvent7: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(Event7) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "event" -> Json.obj(
              "eventType" -> s"Event${Event7.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "paymentDetails" -> Json.obj(
                "amountLumpSum" -> map("lumpSumAmount"),
                "amountCrystalised" -> map("amountCrystallised"),
                "eventDate" -> s"${map("taxYearEndDate")}-04-05"
              )
            )
          )
          )
        )
        )
      val userAnswers = Json.obj(
        s"event${Event7.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")),
              "lumpSumAmount" -> map("lumpSumAmount"),
              "crystallisedAmount" -> map("amountCrystallised"),
              "paymentDate" -> Json.obj(
                "date" -> s"${map("taxYearEndDate")}-04-05"
              ),
            )
          )
        )
      )
      Tuple2(etmpPayload, userAnswers)
    }
  }

  def generateUAFromETMPDataForEvent8: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(Event8) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "event" -> Json.obj(
              "eventType" -> s"Event${Event8.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "paymentDetails" -> Json.obj(
                "amountLumpSum" -> map("lumpSumAmount"),
                "typeOfProtection" -> map("typeOfProtectionEvent8And8A"),
                "eventDate" -> s"${map("taxYearEndDate")}-04-05",
                "freeText" -> map("typeOfProtectionReference")
              )
            )
          )
          )
        )
        )
      val userAnswers = Json.obj(
        s"event${Event8.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")),
              "typeOfProtection" -> map("typeOfProtectionEvent8And8A"),
              "typeOfProtectionReference" -> map("typeOfProtectionReference"),
              "lumpSumAmountAndDate" -> Json.obj(
                "lumpSumAmount" -> map("lumpSumAmount"),
                "lumpSumDate" -> s"${map("taxYearEndDate")}-04-05",
              ),
            )
          )
        )
      )
      Tuple2(etmpPayload, userAnswers)
    }
  }

  def generateUAFromETMPDataForEvent8A: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(Event8A) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "event" -> Json.obj(
              "eventType" -> s"Event${Event8A.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "paymentDetails" -> Json.obj(
                "reasonBenefitTaken" -> map("reasonBenefitTakenEvent8A"),
                "amountLumpSum" -> map("lumpSumAmount"),
                "typeOfProtection" -> map("typeOfProtectionEvent8And8A"),
                "eventDate" -> s"${map("taxYearEndDate")}-04-05",
                "freeText" -> map("typeOfProtectionReference")
              )
            )
          )
          )
        )
        )
      val userAnswers = Json.obj(
        s"event${Event8A.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")),
              "paymentType" -> paymentTypeUAEvent8A(map("reasonBenefitTakenEvent8A")),
              "typeOfProtection" -> map("typeOfProtectionEvent8And8A"),
              "typeOfProtectionReference" -> map("typeOfProtectionReference"),
              "lumpSumAmountAndDate" -> Json.obj(
                "lumpSumAmount" -> map("lumpSumAmount"),
                "lumpSumDate" -> s"${map("taxYearEndDate")}-04-05",
              ),
            )
          )
        )
      )
      Tuple2(etmpPayload, userAnswers)
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
    )
  )

  private def freeTextEvent3(rBT: String): String = if (rBT != "Other") "N/A" else "Example brief description"

  private def paymentTypeUAEvent8A(rBT: String): String = rBT match {
    case "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection"
    => "paymentOfAStandAloneLumpSum"
    case "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance"
    => "paymentOfASchemeSpecificLumpSum"
  }

  private def randomValues(): Gen[Map[String, String]] = {
    for {
      firstName <- Gen.oneOf(Seq("Alice", "Bob", "Charlie"))
      lastName <- Gen.oneOf(Seq("Xavier", "Yilmaz", "Zimmer"))
      nino <- Gen.oneOf(Seq("AB123456C", "CD123456E"))
      deceasedFirstName <- Gen.oneOf(Seq("Derek", "Emma", "Fred"))
      deceasedLastName <- Gen.oneOf(Seq("Stevens", "Thomas", "Jones"))
      deceasedNino <- Gen.oneOf(Seq("ZB123456C", "ZD123456E"))
      reasonBenefitTakenEvent3 <- Gen.oneOf(Seq("Ill Health", "Protected Pension Age", "Other"))
      pensionAmt <- Gen.chooseNum(1, 1000)
      lumpSumAmount <- Gen.chooseNum(1, 1000)
      typeOfProtectionEvent6 <- Gen.oneOf(Seq("Enhanced life time allowance", "Enhanced protection", "Fixed protection", "Fixed protection 2014", "Fixed protection 2016"
        , "Individual protection 2014", "Individual protection 2016"))
      inputProtectionType <- Gen.chooseNum(10000000, 99999999)
      amountCrystallised <- Gen.chooseNum(1, 1000)
      typeOfProtectionEvent8And8A <- Gen.oneOf(Seq("Primary Protection", "Enhanced protection"))
      typeOfProtectionReference <- Gen.chooseNum(10000000, 99999999)
      taxYearEndDate <- Gen.oneOf(2020, 2021, 2022)
      reasonBenefitTakenEvent8A <- Gen.oneOf(
        "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection",
        "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance"
      )
    } yield {
      Map(
        "firstName" -> firstName,
        "lastName" -> lastName,
        "nino" -> nino,
        "deceasedFirstName" -> deceasedFirstName,
        "deceasedLastName" -> deceasedLastName,
        "deceasedNino" -> deceasedNino,
        "reasonBenefitTakenEvent3" -> reasonBenefitTakenEvent3,
        "pensionAmt" -> pensionAmt.toString,
        "amountCrystallised" -> amountCrystallised.toString,
        "lumpSumAmount" -> lumpSumAmount.toString,
        "taxYearEndDate" -> taxYearEndDate.toString,
        "typeOfProtectionEvent6" -> typeOfProtectionEvent6,
        "inputProtectionType" -> inputProtectionType.toString,
        "typeOfProtectionEvent8And8A" -> typeOfProtectionEvent8And8A,
        "typeOfProtectionReference" -> typeOfProtectionReference.toString,
        "reasonBenefitTakenEvent8A" -> reasonBenefitTakenEvent8A
      )
    }
  }
}
