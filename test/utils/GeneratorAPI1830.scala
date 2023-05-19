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
import play.api.libs.json.{JsObject, JsString, Json}

//noinspection ScalaStyle
trait GeneratorAPI1830 extends Matchers with OptionValues with ResponseGenerators {

  import GeneratorAPI1830._

  def generateUserAnswersAndPOSTBodyByEvent(eventType: EventType): Gen[(JsObject, JsObject)] = {
    eventType match {
      case Event2 => generateUserAnswersAndPOSTBodyEvent2
      case Event3 => generateUserAnswersAndPOSTBodyEvent3
      case Event4 => generateUserAnswersAndPOSTBodyEvent4
      case Event5 => generateUserAnswersAndPOSTBodyEvent5 
      case Event6 => generateUserAnswersAndPOSTBodyEvent6
      case Event7 => generateUserAnswersAndPOSTBodyEvent7
      case Event8 => generateUserAnswersAndPOSTBodyEvent8
      case Event8A => generateUserAnswersAndPOSTBodyEvent8A
      case _ => generateUserAnswersAndPOSTBodyEvent22And23(eventType)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent2: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val ua = Json.obj(
        s"event${Event2.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "deceasedMembersDetails" -> Json.obj(
                "firstName" -> map("deceasedFirstName"),
                "lastName" -> map("deceasedLastName"),
                "nino" -> map("deceasedNino")),
              "beneficiaryDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")),
              "amountPaid" -> map("monetaryAmount"),
              "datePaid" -> s"${map("taxYear")}-04-06",
            )
          )
        ),
        "taxYear" -> map("taxYear")
      )

      val expected = Json.obj("memberEventsDetails" -> Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> "87219363YN",
          "eventType" -> s"Event${Event2.toString}",
          "reportStartDate" -> s"${map("taxYear")}-04-06",
          "reportEndDate" -> s"${map("endTaxYear")}-04-05"
        ),
        "eventDetails" -> Json.arr(
          Json.obj(
            "memberDetail" ->
              Json.obj(
                "memberStatus" -> "New",
                "event" ->
                  Json.obj(
                    "eventType" -> s"Event${Event2.toString}",
                    "individualDetails" -> Json.obj(
                      "firstName" -> map("deceasedFirstName"),
                      "lastName" -> map("deceasedLastName"),
                      "nino" -> map("deceasedNino")
                    ),
                    "personReceivedThePayment" -> Json.obj(
                      "firstName" -> map("firstName"),
                      "lastName" -> map("lastName"),
                      "nino" -> map("nino")
                    ),
                    "paymentDetails" -> Json.obj(
                      "amountPaid" -> map("monetaryAmount"),
                      "eventDate" -> s"${map("taxYear")}-04-06"
                    )
                  )
              )
          )
        )
      )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent3: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val ua = Json.obj(
        s"event${Event3.toString}" -> Json.obj(
          "members" ->
            Json.arr(
              Json.obj(
                "membersDetails" -> Json.obj(
                  "firstName" -> map("firstName"),
                  "lastName" -> map("lastName"),
                  "nino" -> map("nino")
                ),
                "benefitType" -> Json.obj(
                  "reasonBenefitTaken" -> map("reasonBenefitTakenEvent3"),
                  "freeText" -> freeTextEvent3(map("reasonBenefitTakenEvent3")),
                ),
                "paymentDetails" -> Json.obj(
                  "amountPaid" -> map("monetaryAmount"),
                  "eventDate" -> s"${map("taxYear")}-04-25",
                ),
              )
            )
        ),
        "taxYear" -> map("taxYear")
      )
      val expected = Json.obj("memberEventsDetails" -> Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> "87219363YN",
          "eventType" -> s"Event${Event3.toString}",
          "reportStartDate" -> s"${map("taxYear")}-04-06",
          "reportEndDate" -> s"${map("endTaxYear")}-04-05"
        ),
        "eventDetails" -> Json.arr(
          Json.obj(
            "memberDetail" ->
              Json.obj(
                "memberStatus" -> "New",
                "event" ->
                  Json.obj(
                    "eventType" -> s"Event${Event3.toString}",
                    "individualDetails" -> Json.obj(
                      "firstName" -> map("firstName"),
                      "lastName" -> map("lastName"),
                      "nino" -> map("nino")
                    ),
                    "paymentDetails" -> Json.obj(
                      "reasonBenefitTaken" -> event3TypeOfBenefitConversion(map("reasonBenefitTakenEvent3")),
                      "amountBenefit" -> map("monetaryAmount"),
                      "eventDate" -> s"${map("taxYear")}-04-25",
                      "freeText" -> freeTextEvent3(map("reasonBenefitTakenEvent3"))
                    )
                  )
              )
          )
        )
      )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent4: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val ua = Json.obj(
        s"event${Event4.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")),
              "paymentDetails" -> Json.obj(
                "amountPaid" -> map("monetaryAmount"),
                "eventDate" -> s"${map("taxYear")}-04-25",
              ),
            )
          )
        ),
        "taxYear" -> map("taxYear")
      )
      val expected = Json.obj("memberEventsDetails" -> Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> "87219363YN",
          "eventType" -> s"Event${Event4.toString}",
          "reportStartDate" -> s"${map("taxYear")}-04-06",
          "reportEndDate" -> s"${map("endTaxYear")}-04-05"
        ),
        "eventDetails" -> Json.arr(
          Json.obj(
            "memberDetail" ->
              Json.obj(
                "memberStatus" -> "New",
                "event" ->
                  Json.obj(
                    "eventType" -> s"Event${Event4.toString}",
                    "individualDetails" -> Json.obj(
                      "firstName" -> map("firstName"),
                      "lastName" -> map("lastName"),
                      "nino" -> map("nino")
                    ),
                    "paymentDetails" -> Json.obj(
                      "amountPaid" -> map("monetaryAmount"),
                      "eventDate" -> s"${map("taxYear")}-04-25"
                    )
                  )
              )
          )
        )
      )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent5: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val ua = Json.obj(
        s"event${Event5.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")),
              "paymentDetails" -> Json.obj(
                "amountPaid" -> map("monetaryAmount"),
                "eventDate" -> s"${map("taxYear")}-04-25",
              ),
            )
          )
        ),
        "taxYear" -> map("taxYear")
      )
      val expected = Json.obj("memberEventsDetails" -> Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> "87219363YN",
          "eventType" -> s"Event${Event5.toString}",
          "reportStartDate" -> s"${map("taxYear")}-04-06",
          "reportEndDate" -> s"${map("endTaxYear")}-04-05"
        ),
        "eventDetails" -> Json.arr(
          Json.obj(
            "memberDetail" ->
              Json.obj(
                "memberStatus" -> "New",
                "event" ->
                  Json.obj(
                    "eventType" -> s"Event${Event5.toString}",
                    "individualDetails" -> Json.obj(
                      "firstName" -> map("firstName"),
                      "lastName" -> map("lastName"),
                      "nino" -> map("nino")
                    ),
                    "paymentDetails" -> Json.obj(
                      "annualRate" -> map("monetaryAmount"),
                      "eventDate" -> s"${map("taxYear")}-04-25"
                    )
                  )
              )
          )
        )
      )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent6: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val ua = Json.obj(
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
                "crystallisedDate" -> s"${map("taxYear")}-04-25"
              )
            )
          )
        ),
        "taxYear" -> map("taxYear")
      )
      val expected = Json.obj("memberEventsDetails" -> Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> "87219363YN",
          "eventType" -> s"Event${Event6.toString}",
          "reportStartDate" -> s"${map("taxYear")}-04-06",
          "reportEndDate" -> s"${map("endTaxYear")}-04-05"
        ),
        "eventDetails" -> Json.arr(
          Json.obj(
            "memberDetail" ->
              Json.obj(
                "memberStatus" -> "New",
                "event" ->
                  Json.obj(
                    "eventType" -> s"Event${Event6.toString}",
                    "individualDetails" -> Json.obj(
                      "firstName" -> map("firstName"),
                      "lastName" -> map("lastName"),
                      "nino" -> map("nino")
                    ),
                    "paymentDetails" -> Json.obj(
                      "amountCrystalised" -> map("amountCrystallised"),
                      "typeOfProtection" -> typeOfProtectionETMPEvent6(map("typeOfProtectionEvent6")),
                      "eventDate" -> s"${map("taxYear")}-04-25",
                      "freeText" -> map("inputProtectionType")
                    )
                  )
              )
          )
        )
      )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent7: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val ua = Json.obj(
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
                "date" -> s"${map("taxYear")}-04-25"
              )
            )
          )
        ),
        "taxYear" -> map("taxYear")
      )
      val expected = Json.obj("memberEventsDetails" -> Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> "87219363YN",
          "eventType" -> s"Event${Event7.toString}",
          "reportStartDate" -> s"${map("taxYear")}-04-06",
          "reportEndDate" -> s"${map("endTaxYear")}-04-05"
        ),
        "eventDetails" -> Json.arr(
          Json.obj(
            "memberDetail" ->
              Json.obj(
                "memberStatus" -> "New",
                "event" ->
                  Json.obj(
                    "eventType" -> s"Event${Event7.toString}",
                    "individualDetails" -> Json.obj(
                      "firstName" -> map("firstName"),
                      "lastName" -> map("lastName"),
                      "nino" -> map("nino")
                    ),
                    "paymentDetails" -> Json.obj(
                      "amountLumpSum" -> map("lumpSumAmount"),
                      "amountCrystalised" -> map("amountCrystallised"),
                      "eventDate" -> s"${map("taxYear")}-04-25"
                    )
                  )
              )
          )
        )
      )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent8: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val ua = Json.obj(
        s"event${Event8.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")),
              "typeOfProtection" -> map("typeOfProtectionEvent8"),
              "typeOfProtectionReference" -> map("typeOfProtectionReference"),
              "lumpSumAmountAndDate" -> Json.obj(
                "lumpSumAmount" -> map("lumpSumAmount"),
                "lumpSumDate" -> s"${map("taxYear")}-04-25"
              )
            )
          )
        ),
        "taxYear" -> map("taxYear")
      )
      val expected = Json.obj("memberEventsDetails" -> Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> "87219363YN",
          "eventType" -> s"Event${Event8.toString}",
          "reportStartDate" -> s"${map("taxYear")}-04-06",
          "reportEndDate" -> s"${map("endTaxYear")}-04-05"
        ),
        "eventDetails" -> Json.arr(
          Json.obj(
            "memberDetail" ->
              Json.obj(
                "memberStatus" -> "New",
                "event" ->
                  Json.obj(
                    "eventType" -> s"Event${Event8.toString}",
                    "individualDetails" -> Json.obj(
                      "firstName" -> map("firstName"),
                      "lastName" -> map("lastName"),
                      "nino" -> map("nino")
                    ),
                    "paymentDetails" -> Json.obj(
                      "amountLumpSum" -> map("lumpSumAmount"),
                      "typeOfProtection" -> typeOfProtectionETMPEvent8(map("typeOfProtectionEvent8")),
                      "eventDate" -> s"${map("taxYear")}-04-25",
                      "freeText" -> map("typeOfProtectionReference")
                    )
                  )
              )
          )
        )
      )
      )
      Tuple2(ua, expected)
    }
  }

  def generateUserAnswersAndPOSTBodyEvent8A: Gen[(JsObject, JsObject)] = for {
    map <- randomValues()
  } yield {
    val optionalKeysAndValuesUA: Option[JsObject] = if (map("paymentTypeEvent8A") == "paymentOfAStandAloneLumpSum") {
      val keysAndValues = Json.obj(
        "typeOfProtection" -> JsString(map("typeOfProtectionEvent8")),
        "typeOfProtectionReference" -> JsString(map("typeOfProtectionReference"))
      )
      Some(keysAndValues)
    } else {
      Some(JsObject.empty)
    }

    val ua = Json.obj(
      s"event${Event8A.toString}" -> Json.obj("members" ->
        Json.arr(
          Json.obj(
            "membersDetails" -> Json.obj(
              "firstName" -> map("firstName"),
              "lastName" -> map("lastName"),
              "nino" -> map("nino")),
            "paymentType" -> map("paymentTypeEvent8A"),
            "lumpSumAmountAndDate" -> Json.obj(
              "lumpSumAmount" -> map("lumpSumAmount"),
              "lumpSumDate" -> s"${map("taxYear")}-04-25"
            )
          ).++(optionalKeysAndValuesUA.get)
        )
      ),
      "taxYear" -> map("taxYear")
    )
    val keysAndValuesETMP: JsObject = if (map("paymentTypeEvent8A") == "paymentOfAStandAloneLumpSum") {
      Json.obj(
        "typeOfProtection" -> typeOfProtectionETMPEvent8A(map("typeOfProtectionEvent8")),
        "freeText" -> map("typeOfProtectionReference")
      )
    } else {
      Json.obj(
        "typeOfProtection" -> "N/A",
        "freeText" -> "N/A"
      )
    }

    val expected = Json.obj("memberEventsDetails" -> Json.obj(
      "eventReportDetails" -> Json.obj(
        "pSTR" -> "87219363YN",
        "eventType" -> s"Event${Event8A.toString}",
        "reportStartDate" -> s"${map("taxYear")}-04-06",
        "reportEndDate" -> s"${map("endTaxYear")}-04-05"
      ),
      "eventDetails" -> Json.arr(
        Json.obj(
          "memberDetail" ->
            Json.obj(
              "memberStatus" -> "New",
              "event" -> Json.obj(
                  "eventType" -> s"Event${Event8A.toString}",
                  "individualDetails" -> Json.obj(
                    "firstName" -> map("firstName"),
                    "lastName" -> map("lastName"),
                    "nino" -> map("nino")
                  ),
                  "paymentDetails" -> Json.obj(
                    "reasonBenefitTaken" -> paymentTypeETMPEvent8A(map("paymentTypeEvent8A")),
                    "amountLumpSum" -> map("lumpSumAmount"),
                    "eventDate" -> s"${map("taxYear")}-04-25"
                  ).++(keysAndValuesETMP)
                )
            )
        )
      )
    )
    )
    Tuple2(ua, expected)
  }

  def generateUserAnswersAndPOSTBodyEvent22And23(eventType: EventType): Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val ua = Json.obj(
        s"event${eventType.toString}" -> Json.obj("members" ->
          Json.arr(
            Json.obj(
              "membersDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")),
              "chooseTaxYear" -> map("taxYear"),
              "totalPensionAmounts" -> map("monetaryAmount")
            ))
        ),
        "taxYear" -> map("taxYear")
      )
      val expected = Json.obj("memberEventsDetails" -> Json.obj(
        "eventReportDetails" -> Json.obj(
          "pSTR" -> "87219363YN",
          "eventType" -> s"Event${eventType.toString}",
          "reportStartDate" -> s"${map("taxYear")}-04-06",
          "reportEndDate" -> s"${map("endTaxYear")}-04-05"
        ),
        "eventDetails" -> Json.arr(
          Json.obj(
            "memberDetail" ->
              Json.obj(
                "memberStatus" -> "New",
                "event" ->
                  Json.obj(
                    "eventType" -> s"Event${eventType.toString}",
                    "individualDetails" -> Json.obj(
                      "firstName" -> map("firstName"),
                      "lastName" -> map("lastName"),
                      "nino" -> map("nino")
                    ),
                    "paymentDetails" -> Json.obj(
                      "monetaryAmount" -> map("monetaryAmount"),
                      "taxYearEndingDate" -> s"${map("endTaxYear")}-04-05"
                    )
                  )
              )
          )
        )
      )
      )
      Tuple2(ua, expected)
    }
  }
}

//noinspection ScalaStyle
object GeneratorAPI1830 {
  private val typesOfProtectionUAEvent6 = Seq(
    "enhancedLifetimeAllowance",
    "enhancedProtection",
    "fixedProtection",
    "fixedProtection2014",
    "fixedProtection2016",
    "individualProtection2014",
    "individualProtection2016"
  )

  private val typesOfProtectionUAEvent8And8A = Seq(
    "primaryProtection",
    "enhancedProtection"
  )

  private val paymentTypesUAEvent8A = Seq(
    "paymentOfAStandAloneLumpSum",
    "paymentOfASchemeSpecificLumpSum"
  )

  private def typeOfProtectionETMPEvent6(tOP: String): String = tOP match {
    case "enhancedLifetimeAllowance" => "Enhanced life time allowance"
    case "enhancedProtection" => "Enhanced protection"
    case "fixedProtection" => "Fixed protection"
    case "fixedProtection2014" => "Fixed protection 2014"
    case "fixedProtection2016" => "Fixed protection 2016"
    case "individualProtection2014" => "Individual protection 2014"
    case "individualProtection2016" => "Individual protection 2016"
  }

  private def typeOfProtectionETMPEvent8(tOP: String): String = tOP match {
    case "primaryProtection" => "Primary Protection"
    case "enhancedProtection" => "Enhanced protection"
  }

  private def typeOfProtectionETMPEvent8A(tOP: String): String = tOP match {
    case "primaryProtection" => "Primary Protection"
    case "enhancedProtection" => "Enhanced"
  }

  private def paymentTypeETMPEvent8A(pT: String): String = pT match {
    case "paymentOfAStandAloneLumpSum" =>
      "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection"
    case "paymentOfASchemeSpecificLumpSum" =>
      "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance"
  }

  private def freeTextEvent3(rBT: String): String = if (rBT != "other") "N/A" else "Example brief description"

  private def event3TypeOfBenefitConversion(tOB: String): String = tOB match {
    case "illHealth" => "Ill Health"
    case "protectedPensionAge" => "Protected Pension Age"
    case "other" => "Other"
  }

  private def randomValues(): Gen[Map[String, String]] = {
    for {
      firstName <- Gen.oneOf(Seq("Alice", "Bob", "Charlie"))
      lastName <- Gen.oneOf(Seq("Xavier", "Yilmaz", "Zimmer"))
      nino <- Gen.oneOf(Seq("AB123456C", "CD123456E"))
      typeOfProtectionEvent6 <- Gen.oneOf(typesOfProtectionUAEvent6)
      typeOfProtectionEvent8And8A <- Gen.oneOf(typesOfProtectionUAEvent8And8A)
      inputProtectionType <- Gen.chooseNum(10000000, 99999999)
      typeOfProtectionReference <- Gen.chooseNum(10000000, 99999999)
      amountCrystallised <- Gen.chooseNum(1, 1000)
      lumpSumAmount <- Gen.chooseNum(1, 1000)
      taxYear <- Gen.oneOf(Seq("2022", "2023", "2024"))
      endTaxYear = (taxYear.toInt + 1)
      monetaryAmount <- Gen.chooseNum(1, 1000)
      taxYearEndDate <- Gen.oneOf(2020, 2021, 2022)
      paymentTypeEvent8A <- Gen.oneOf(paymentTypesUAEvent8A)
      deceasedFirstName <- Gen.oneOf(Seq("Daniel", "Emma", "Fred"))
      deceasedLastName <- Gen.oneOf(Seq("Urqhart", "Vanderbilt", "Wilson"))
      deceasedNino <- Gen.oneOf(Seq("AB654321C", "CD654321E"))
      reasonBenefitTakenEvent3 <- Gen.oneOf(Seq("illHealth", "protectedPensionAge", "other"))
    } yield {
      Map(
        "firstName" -> firstName,
        "lastName" -> lastName,
        "nino" -> nino,
        "typeOfProtectionEvent6" -> typeOfProtectionEvent6,
        "typeOfProtectionEvent8" -> typeOfProtectionEvent8And8A,
        "inputProtectionType" -> inputProtectionType.toString,
        "typeOfProtectionReference" -> typeOfProtectionReference.toString,
        "amountCrystallised" -> amountCrystallised.toString,
        "lumpSumAmount" -> lumpSumAmount.toString,
        "taxYear" -> taxYear,
        "endTaxYear" -> endTaxYear.toString,
        "monetaryAmount" -> monetaryAmount.toString,
        "taxYearEndDate" -> taxYearEndDate.toString,
        "paymentTypeEvent8A" -> paymentTypeEvent8A,
        "deceasedFirstName" -> deceasedFirstName,
        "deceasedLastName" -> deceasedLastName,
        "deceasedNino" -> deceasedNino,
        "reasonBenefitTakenEvent3" -> reasonBenefitTakenEvent3
      )
    }
  }
}
