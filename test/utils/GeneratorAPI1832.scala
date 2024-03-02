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

import models.enumeration.EventType
import models.enumeration.EventType.{Event2, Event24, Event3, Event4, Event5, Event6, Event7, Event8, Event8A}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsBoolean, JsObject, JsString, Json}


//noinspection ScalaStyle
trait GeneratorAPI1832 extends Matchers with OptionValues with ResponseGenerators {

  import utils.GeneratorAPI1832._

  def generateUserAnswersAndPOSTBodyByEvent(eventType: EventType): Gen[(JsObject, JsObject)] = {
    eventType match {
      case Event2 => generateUAFromETMPDataForEvent2
      case Event3 => generateUAFromETMPDataForEvent3
      case Event4 => generateUAFromETMPDataForEvent4
      case Event5 => generateUAFromETMPDataForEvent5
      case Event6 => generateUAFromETMPDataForEvent6
      case Event7 => generateUAFromETMPDataForEvent7
      case Event8 => generateUAFromETMPDataForEvent8
      case Event8A => generateUAFromETMPDataForEvent8A
      case Event24 => generateUAFromETMPDataForEvent24
      case _ => generateUAFromETMPDataForEvent22And23(eventType)
    }
  }

  private def generateUAFromETMPDataForEvent2: Gen[(JsObject, JsObject)] = {
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
              "memberStatus" -> "New",
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

  private def generateUAFromETMPDataForEvent3: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {


      val jsonFreeText = freeTextEvent3(map("reasonBenefitTakenEvent3")) match {
        case Some(v) => Json.obj("freeText" -> v)
        case None => Json.obj()
      }

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
              "paymentDetails" -> (Json.obj(
                "reasonBenefitTaken" -> map("reasonBenefitTakenEvent3"),
                "amountBenefit" -> map("pensionAmt"),
                "eventDate" -> s"${map("taxYearEndDate")}-04-05"
              ) ++ jsonFreeText)
            )
          )
          )
        )
        )

      val userAnswers = Json.obj(
        s"event${Event3.toString}" -> Json.obj("members" -> Json.arr(
          Json.obj(
            "memberStatus" -> "New",
            "membersDetails" -> Json.obj(
              "firstName" -> map("firstName"),
              "lastName" -> map("lastName"),
              "nino" -> map("nino")),
            "benefitType" -> (Json.obj(
              "reasonBenefitTaken" -> reasonBenefitTakenUAEvent3(map("reasonBenefitTakenEvent3"))
            ) ++ jsonFreeText),
            "paymentDetails" -> Json.obj(
              "amountPaid" -> map("pensionAmt"),
              "eventDate" -> s"${map("taxYearEndDate")}-04-05"
            ),
          )
        )
        )
      )
      Tuple2(etmpPayload, userAnswers)
    }
  }

  private def generateUAFromETMPDataForEvent4: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(Event4) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "memberStatus" -> "New",
            "event" -> Json.obj(
              "eventType" -> s"Event${Event4.toString}",
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
        s"event${Event4.toString}" -> Json.obj("members" -> Json.arr(
          Json.obj(
            "memberStatus" -> "New",
            "membersDetails" -> Json.obj(
              "firstName" -> map("firstName"),
              "lastName" -> map("lastName"),
              "nino" -> map("nino")),
            "paymentDetails" -> Json.obj(
              "amountPaid" -> map("pensionAmt"),
              "eventDate" -> s"${map("taxYearEndDate")}-04-05"
            ),
          )
        )
        )
      )
      Tuple2(etmpPayload, userAnswers)
    }
  }

  private def generateUAFromETMPDataForEvent5: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(Event5) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "memberStatus" -> "New",
            "event" -> Json.obj(
              "eventType" -> s"Event${Event5.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "paymentDetails" -> Json.obj(
                "annualRate" -> map("pensionAmt"),
                "eventDate" -> s"${map("taxYearEndDate")}-04-05"
              )
            )
          )
          )
        )
        )
      val userAnswers = Json.obj(
        s"event${Event5.toString}" -> Json.obj("members" -> Json.arr(
          Json.obj(
            "memberStatus" -> "New",
            "membersDetails" -> Json.obj(
              "firstName" -> map("firstName"),
              "lastName" -> map("lastName"),
              "nino" -> map("nino")),
            "paymentDetails" -> Json.obj(
              "amountPaid" -> map("pensionAmt"),
              "eventDate" -> s"${map("taxYearEndDate")}-04-05"
            ),
          )
        )
        )
      )
      Tuple2(etmpPayload, userAnswers)
    }
  }

  private def generateUAFromETMPDataForEvent6: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(Event6) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "memberStatus" -> "New",
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
        s"event${Event6.toString}" -> Json.obj("members" -> Json.arr(
          Json.obj(
            "memberStatus" -> "New",
            "membersDetails" -> Json.obj(
              "firstName" -> map("firstName"),
              "lastName" -> map("lastName"),
              "nino" -> map("nino")),
            "typeOfProtection" -> typeOfProtectionUAEvent6(map("typeOfProtectionEvent6")),
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

  private def generateUAFromETMPDataForEvent7: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(Event7) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "memberStatus" -> "New",
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
        s"event${Event7.toString}" -> Json.obj("members" -> Json.arr(
          Json.obj(
            "memberStatus" -> "New",
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

  private def generateUAFromETMPDataForEvent8: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(Event8) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "memberStatus" -> "New",
            "event" -> Json.obj(
              "eventType" -> s"Event${Event8.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "paymentDetails" -> Json.obj(
                "amountLumpSum" -> map("lumpSumAmount"),
                "typeOfProtection" -> map("typeOfProtectionEvent8"),
                "eventDate" -> s"${map("taxYearEndDate")}-04-05",
                "freeText" -> map("typeOfProtectionReference")
              )
            )
          )
          )
        )
        )
      val userAnswers = Json.obj(
        s"event${Event8.toString}" -> Json.obj("members" -> Json.arr(
          Json.obj(
            "memberStatus" -> "New",
            "membersDetails" -> Json.obj(
              "firstName" -> map("firstName"),
              "lastName" -> map("lastName"),
              "nino" -> map("nino")),
            "typeOfProtection" -> typeOfProtectionUAEvent8(map("typeOfProtectionEvent8")),
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

  private def toJsonObject(s: String, fieldName: String, f: String => String = identity) = s match {
    case "" => Json.obj()
    case t => Json.obj(fieldName -> f(t))
  }

  private def generateUAFromETMPDataForEvent8A: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val typeOfProtectionJson = toJsonObject(map("typeOfProtectionEvent8A"), "typeOfProtection")
      val reasonBenefitTakenJson = toJsonObject(map("reasonBenefitTakenEvent8A"), "reasonBenefitTaken")
      val freeTextJson = toJsonObject(map("typeOfProtectionReference8A"), "freeText")
      val etmpPayload = etmpData(Event8A) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "memberStatus" -> "New",
            "event" -> Json.obj(
              "eventType" -> s"Event${Event8A.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "paymentDetails" -> (Json.obj(
                "amountLumpSum" -> map("lumpSumAmount"),
                "eventDate" -> s"${map("taxYearEndDate")}-04-05"
              ) ++ reasonBenefitTakenJson ++ typeOfProtectionJson ++ freeTextJson)
            )
          )
          )
        )
        )

      val typeOfProtectionUAJson = toJsonObject(map("typeOfProtectionEvent8A"), "typeOfProtection", typeOfProtectionUAEvent8A)
      val reasonBenefitTakenUAJson = toJsonObject(map("reasonBenefitTakenEvent8A"), "paymentType", paymentTypeUAEvent8A)
      val typeOfProtectionReferenceUAJson = toJsonObject(map("typeOfProtectionReference8A"), "typeOfProtectionReference")

      val userAnswers = Json.obj(
        s"event${Event8A.toString}" -> Json.obj("members" -> Json.arr(
          Json.obj(
            "memberStatus" -> "New",
            "membersDetails" -> Json.obj(
              "firstName" -> map("firstName"),
              "lastName" -> map("lastName"),
              "nino" -> map("nino")),
            "lumpSumAmountAndDate" -> Json.obj(
              "lumpSumAmount" -> map("lumpSumAmount"),
              "lumpSumDate" -> s"${map("taxYearEndDate")}-04-05",
            ),
          ) ++ typeOfProtectionUAJson ++ reasonBenefitTakenUAJson ++ typeOfProtectionReferenceUAJson
        )
        )
      )
      Tuple2(etmpPayload, userAnswers)
    }
  }

  private def generateUAFromETMPDataForEvent22And23(eventType: EventType): Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val etmpPayload = etmpData(eventType) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "memberStatus" -> "New",
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
              "memberStatus" -> "New",
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

  private def generateUAFromETMPDataForEvent24: Gen[(JsObject, JsObject)] = {
    for {
      map <- randomValues()
    } yield {
      val overAllowanceAndDeathBenefitETMP: JsObject = if (map("overAllowance") == "No") {
        Json.obj(
          "availableLumpSumDBAExceeded" -> map("overAllowanceAndDeathBenefit"),
        )
      } else {
        Json.obj()
      }

      val marginalRateETMP: JsObject = if (map("overAllowance") == "Yes" || map("overAllowanceAndDeathBenefit") == "Yes") {
        Json.obj(
          "taxedAtMarginalRate" -> map("marginalRate"),
        )
      } else {
        Json.obj()
      }

      val payeRefETMP: JsObject = if (map("overAllowance") == "Yes" && map("marginalRate") == "Yes" ||
        map("overAllowanceAndDeathBenefit") == "Yes" && map("marginalRate") == "Yes") {
        Json.obj(
          "payeReference" -> JsString("123/AB123"),
        )
      } else {
        Json.obj()
      }

      val schemeSpecificETMP: JsObject = if (map("typeOfProtectionGroup1") == "schemeSpecific") {
        Json.obj(
          "schemeSpecificLumpSum" -> JsString("Yes"),
        )
      } else {
        Json.obj()
      }

      val typeOfProtectionJson = toJsonObject(map("typeOfProtectionGroup2"), "typeOfProtection")
      val reasonBenefitTakenJson = toJsonObject(map("reasonBenefitTakenEvent24"), "reasonBenefitTaken")

      def yesNoToBool(str: String): Boolean = str == "Yes"

      val etmpPayload = etmpData(Event24) ++
        Json.obj("eventDetails" -> Json.arr(
          Json.obj("memberDetail" -> Json.obj(
            "memberStatus" -> "New",
            "event" -> Json.obj(
              "eventType" -> s"Event${Event24.toString}",
              "individualDetails" -> Json.obj(
                "firstName" -> map("firstName"),
                "lastName" -> map("lastName"),
                "nino" -> map("nino")
              ),
              "paymentDetails" -> (Json.obj(
                "memberHoldProtection" -> map("validProtection"),
                "preCommenceReference" -> map("typeOfProtectionReference"),
                "pensionCreditReference" -> map("typeOfProtectionReference"),
                "nonResidenceReference" -> map("typeOfProtectionReference"),
                "overseasReference" -> map("typeOfProtectionReference"),
                "availableLumpSumExceeded" -> map("overAllowance"),
                "amountCrystalised" -> map("lumpSumAmount")) ++
                typeOfProtectionJson ++
                reasonBenefitTakenJson ++
                Json.obj(
                "taxYearEndingDate" -> "2023-09-22",
                "freeText" -> map("typeOfProtectionReference")
              ).++(overAllowanceAndDeathBenefitETMP)
                .++(marginalRateETMP)
                .++(payeRefETMP)
                .++(schemeSpecificETMP)
              )
            )
          ))
        ))

      val optionalOverAllowanceAndDeathBenefit: Option[JsObject] = if (map("overAllowance") == "No") {
        val keyAndValue = Json.obj(
          "overAllowanceAndDeathBenefit" -> JsString(map("overAllowanceAndDeathBenefit")),
        )
        Some(keyAndValue)
      } else {
        Some(JsObject.empty)
      }

      val optionalMarginalRate: Option[JsObject] = if (
        map("overAllowance") == "Yes" || map("overAllowanceAndDeathBenefit") == "Yes"
      ) {
        val keyAndValue = Json.obj(
          "marginalRate" ->  JsBoolean(yesNoToBool(map("marginalRate"))),
        )
        Some(keyAndValue)
      } else {
        Some(JsObject.empty)
      }

      val optionalPayeRef: Option[JsObject] = if (
        map("overAllowance") == "Yes" && map("marginalRate") == "Yes" ||
          map("overAllowanceAndDeathBenefit") == "Yes" && map("marginalRate") == "Yes"
      ) {
        val keyAndValue = Json.obj(
          "employerPayeReference" -> JsString("123/AB123"),
        )
        Some(keyAndValue)
      } else {
        Some(JsObject.empty)
      }

      val optionalSchemeSpecificLumpSum: Option[JsObject] = if (
        map("typeOfProtectionGroup1").contains("schemeSpecific")
      ) {
        val keyAndValue = Json.obj(
          "schemeSpecificLumpSum" -> JsBoolean(true),
        )
        Some(keyAndValue)
      } else {
        Some(JsObject.empty)
      }

      val typeOfProtectionUAJson = toJsonObject(map("typeOfProtectionGroup2"), "typeOfProtectionGroup2", typeOfProtectionEvent24)
      val reasonBenefitTakenUAJson = toJsonObject(map("reasonBenefitTakenEvent24"), "bceTypeSelection", reasonBenefitTakenEvent24)

      val userAnswers = Json.obj(
        s"event${Event24.toString}" -> Json.obj("members" -> Json.arr(
          Json.obj(
            "membersDetails" -> Json.obj(
              "firstName" -> map("firstName"),
              "lastName" -> map("lastName"),
              "nino" -> map("nino")
            ),
            "memberStatus" -> "New",
            "validProtection" -> yesNoToBool(map("validProtection"))) ++
            typeOfProtectionUAJson ++
            Json.obj("typeOfProtectionGroup1Reference" -> Json.obj(
              "preCommencement" -> map("typeOfProtectionReference"),
              "pensionCreditsPreCRE" -> map("typeOfProtectionReference"),
              "nonResidenceEnhancement" -> map("typeOfProtectionReference"),
              "recognisedOverseasPSTE" -> map("typeOfProtectionReference"),
            ),
            "overAllowance" -> yesNoToBool(map("overAllowance"))
          )++ optionalOverAllowanceAndDeathBenefit.get
            ++ optionalSchemeSpecificLumpSum.get ++
          Json.obj(
            "totalAmountBenefitCrystallisation" -> map("lumpSumAmount")
          ) ++
            reasonBenefitTakenUAJson ++
            Json.obj("crystallisedDate" -> Json.obj(
              "date" -> "2023-09-22"
            ),
            "typeOfProtectionGroup2Reference" -> map("typeOfProtectionReference"),
          )  ++
            optionalMarginalRate.get ++
            optionalPayeRef.get
        ))
      )
      Tuple2(etmpPayload, userAnswers)
    }
  }
}

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

  private def typeOfProtectionUAEvent6(tOP: String): String = tOP match {
    case "Enhanced life time allowance" => "enhancedLifetimeAllowance"
    case "Enhanced protection" => "enhancedProtection"
    case "Fixed protection" => "fixedProtection"
    case "Fixed protection 2014" => "fixedProtection2014"
    case "Fixed protection 2016" => "fixedProtection2016"
    case "Individual protection 2014" => "individualProtection2014"
    case "Individual protection 2016" => "individualProtection2016"
  }

  private def typeOfProtectionUAEvent8(tOP: String): String = tOP match {
    case "Primary Protection" => "primaryProtection"
    case "Enhanced protection" => "enhancedProtection"
  }

  private def typeOfProtectionUAEvent8A(tOP: String): String = tOP match {
    case "Primary Protection" => "primaryProtection"
    case "Enhanced protection" => "enhancedProtection"
  }

  private def typeOfProtectionEvent24(tOP: String): String = tOP match {
    case "Enhanced protection" => "enhancedProtection"
    case "Enhanced protection with protected lump sum rights of more than 375,000" => "enhancedProtectionWithProtectedSum"
    case "Fixed protection" => "fixedProtection"
    case "Fixed protection 2014" => "fixedProtection2014"
    case "Fixed protection 2016" => "fixedProtection2016"
    case "Individual protection 2014" => "individualProtection2014"
    case "Individual protection 2016" => "individualProtection2016"
    case "Primary protection" => "primaryProtection"
    case "Primary protection with protected lump sum rights of more than 375,000" => "primaryWithProtectedSum"
  }

  //noinspection ScalaStyle
  private def reasonBenefitTakenEvent24(tOB: String): String = tOB match {
    case "An annuity protection lump sum death benefit" => "annuityProtection"
    case "A defined benefit lump sum death benefit" => "definedBenefit"
    case "A drawdown pension fund lump sum death benefit" => "drawdown"
    case "A flexi-access drawdown lump sum death benefit" => "flexiAccess"
    case "A pension protection lump sum death benefit" => "pensionProtection"
    case "A small lump sum" => "small"
    case "A stand-alone lump sum" => "standAlone"
    case "A trivial commutation lump sum" => "trivialCommutation"
    case "Serious ill health lump sum" => "seriousHealthLumpSum"
    case "An uncrystalised funds pension lump sum" => "uncrystallisedFunds"
    case "A uncrystallised funds lump sum death benefit" => "uncrystallisedFundsDeathBenefit"
    case "A winding-up lump sum" => "windingUp"
  }

  private def freeTextEvent3(rBT: String): Option[String] = if (rBT != "Other") None else Some("Example brief description")

  private def reasonBenefitTakenUAEvent3(rBT: String): String = rBT match {
    case "Ill Health" => "illHealth"
    case "Protected Pension Age" => "protectedPensionAge"
    case "Other" => "other"
  }

  private def paymentTypeUAEvent8A(rBT: String): String = rBT match {
    case "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection"
    => "paymentOfAStandAloneLumpSum"
    case "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance"
    => "paymentOfASchemeSpecificLumpSum"
  }

  private val reasonBenefitTakenTypesEvent24 = Seq(
   "An annuity protection lump sum death benefit",
   "A defined benefit lump sum death benefit",
   "A drawdown pension fund lump sum death benefit",
   "A flexi-access drawdown lump sum death benefit",
   "A pension protection lump sum death benefit",
   "A small lump sum",
   "A stand-alone lump sum",
   "A trivial commutation lump sum",
   "Serious ill health lump sum",
   "An uncrystalised funds pension lump sum",
   "A uncrystallised funds lump sum death benefit",
   "A winding-up lump sum"
  )

  private val protectionTypesGroup1Event24 = Seq(
    "nonResidenceEnhancement",
    "pensionCreditsPreCRE",
    "preCommencement",
    "recognisedOverseasPSTE",
    "schemeSpecific"
  )

  private val protectionTypesGroup2Event24 = Seq(
   "Enhanced protection",
   "Enhanced protection with protected lump sum rights of more than 375,000",
   "Fixed protection",
   "Fixed protection 2014",
   "Fixed protection 2016",
   "Individual protection 2014",
   "Individual protection 2016",
   "Primary protection",
   "Primary protection with protected lump sum rights of more than 375,000"
  )

  //noinspection ScalaStyle
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
      typeOfProtectionEvent6 <- Gen.oneOf(Seq("Enhanced life time allowance",
        "Enhanced protection", "Fixed protection", "Fixed protection 2014", "Fixed protection 2016",
        "Individual protection 2014", "Individual protection 2016"))
      inputProtectionType <- Gen.chooseNum(10000000, 99999999)
      amountCrystallised <- Gen.chooseNum(1, 1000)
      typeOfProtectionEvent8 <- Gen.oneOf(Seq("Primary Protection", "Enhanced protection"))
      typeOfProtectionEvent8A <- Gen.oneOf(Seq("Primary Protection", "Enhanced protection", ""))
      typeOfProtectionReference <- Gen.chooseNum(10000000, 99999999)
      typeOfProtectionReference8A <- Gen.oneOf("10000000", "99999999", "")
      taxYearEndDate <- Gen.oneOf(2020, 2021, 2022)
      endTaxYear = (taxYearEndDate + 1)
      reasonBenefitTakenEvent8A <- Gen.oneOf(
        "Member where payment of a stand-alone lump sum (100 per lump sum) and the member had protected lump sum rights of more than £375,000 with either primary protection or enhanced protection",
        "Member where payment of a scheme specific lump sum protection and the lump sum is more than 7.5 per of the lifetime allowance",
        ""
      )
      validProtection <- Gen.oneOf(Seq("Yes", "No"))
      overAllowance <- Gen.oneOf(Seq("Yes", "No"))
      overAllowanceAndDeathBenefit <- Gen.oneOf(Seq("Yes", "No"))
      marginalRate <- Gen.oneOf(Seq("Yes", "No"))
      reasonBenefitTakenEvent24 <- Gen.oneOf(reasonBenefitTakenTypesEvent24)
      typeOfProtectionGroup1 <- Gen.oneOf(protectionTypesGroup1Event24)
      typeOfProtectionGroup2 <- Gen.oneOf(protectionTypesGroup2Event24)
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
        "endTaxYear" -> endTaxYear.toString,
        "typeOfProtectionEvent6" -> typeOfProtectionEvent6,
        "inputProtectionType" -> inputProtectionType.toString,
        "typeOfProtectionEvent8" -> typeOfProtectionEvent8,
        "typeOfProtectionEvent8A" -> typeOfProtectionEvent8A,
        "typeOfProtectionReference" -> typeOfProtectionReference.toString,
        "typeOfProtectionReference8A" -> typeOfProtectionReference8A,
        "reasonBenefitTakenEvent8A" -> reasonBenefitTakenEvent8A,
        "validProtection" -> validProtection,
        "overAllowance" -> overAllowance,
        "overAllowanceAndDeathBenefit" -> overAllowanceAndDeathBenefit,
        "marginalRate" -> marginalRate,
        "reasonBenefitTakenEvent24" -> reasonBenefitTakenEvent24,
        "typeOfProtectionGroup1" -> typeOfProtectionGroup1,
        "typeOfProtectionGroup2" -> typeOfProtectionGroup2
      )
    }
  }
}
