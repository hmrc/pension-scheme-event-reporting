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

package transformations.ETMPToFrontEnd

import models.enumeration.EventType
import models.enumeration.EventType.{Event10, Event11, Event12, Event13, Event20, WindUp}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.language.implicitConversions


object API1834 {

  implicit def reads(eventType: EventType): Reads[JsObject] = {
    eventType match {
      case Event10 => event10Reads
      case Event11 => event11Reads
      case Event12 => event12Reads
      case Event13 => event13Reads
      case Event20 => event20Reads
      case WindUp => eventWindUpReads
      case e => Reads.failed(s"Unknown event type $e")
    }
  }

  /*
  val payload: JsObject = Json.obj(
    "eventDetails" -> Json.obj(
      "event12" -> Json.obj(
        "recordVersion" -> "001",
        "twoOrMoreSchemesDate" -> date
      )
    )
  )

  val expected = Json.obj(
    "event12" -> Json.obj(
      "hasSchemeChangedRules" -> true,
      "dateOfChange" -> Json.obj {
        "dateOfChange" -> date
      }
    )
  )
   */

  private val event20Reads: Reads[JsObject] = {

    val startDateOpt = (__ \ "eventDetails" \ "event20" \ "occSchemeDetails"\ "startDateOfOccScheme").readNullable[String]
    val ceaseDateOpt = (__ \ "eventDetails" \ "event20" \ "occSchemeDetails"\ "stopDateOfOccScheme").readNullable[String]

    startDateOpt.map {
      case Some(data) => ((__ \ "event20" \ "becameDate"\ "date").json.put(JsString(data)) and
        (__ \ "event20" \ "whatChange").json.put(JsString("becameOccupationalScheme")
      )).reduce
      case _ => ceaseDateOpt.map{
        case Some(data) => ((__ \ "event20" \ "ceasedDate"\ "date").json.put(JsString(data)) and
          (__ \ "event20" \ "whatChange").json.put(JsString("ceasedOccupationalScheme")
          )).reduce
        case _ => Reads.pure(Json.obj())
      }.flatMap(identity)
    }.flatMap(identity)
  }

  private val eventWindUpReads: Reads[JsObject] = {
    (__ \ "eventDetails" \ "eventWindUp" \ "dateOfWindUp").readNullable[String].map {
      case Some(data) => (__ \ "eventWindUp" \ "schemeWindUpDate").json.put(JsString(data))
      case _ => Reads.pure(Json.obj())
    }.flatMap(identity)
  }

  private val event12Reads: Reads[JsObject] = {
    (__ \ "eventDetails" \ "event12" \ "twoOrMoreSchemesDate").readNullable[String].map {
      case Some(date) =>
        ((__ \ "event12" \ "hasSchemeChangedRules").json.put(JsBoolean(true)) and
          (__ \ "event12" \ "dateOfChange" \ "dateOfChange").json.put(JsString(date))).reduce
      case _ => Reads.pure(Json.obj())
    }.flatMap(identity)
  }

  private val event13Reads: Reads[JsObject] = {

    def mapStructure(s: String) = s match {
      case "A single trust under which all of the assets are held for the benefit of all members of the scheme" => "single"
      case "A group life/death in service scheme" => "group"
      case "A body corporate" => "corporate"
      case "Other" => "other"
    }

    (
      (__ \ "eventDetails" \ "event13" \ "dateOfChange").readNullable[String] and
        (__ \ "eventDetails" \ "event13" \ "schemeStructure").readNullable[String]
      ) (
      (date, structure) => {
        (date, structure) match {
          case (Some(d), Some(s)) =>
            (
              (__ \ "event13" \ "schemeStructure").json.put(JsString(mapStructure(s))) and
                (__ \ "event13" \ "changeDate").json.put(JsString(d))
              ).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)
  }

  private val event11Reads: Reads[JsObject] = {
    (
      (__ \ "eventDetails" \ "event11" \ "unauthorisedPmtsDate").readNullable[String] and
        (__ \ "eventDetails" \ "event11" \ "contractsOrPoliciesDate").readNullable[String]
      ) (
      (unAuthDate, contractsDate) => {
        (unAuthDate, contractsDate) match {
          case (Some(date1), Some(date2)) =>
            (
              (__ \ "event11" \ "hasSchemeChangedRulesUnAuthPayments").json.put(JsBoolean(true)) and
                (__ \ "event11" \ "unAuthPaymentsRuleChangeDate").json.put(JsString(date1)) and
                (__ \ "event11" \ "hasSchemeChangedRulesInvestmentsInAssets").json.put(JsBoolean(true)) and
                (__ \ "event11" \ "investmentsInAssetsRuleChangeDate").json.put(JsString(date2))
              ).reduce
          case (Some(date1), _) =>
            (
              (__ \ "event11" \ "hasSchemeChangedRulesUnAuthPayments").json.put(JsBoolean(true)) and
                (__ \ "event11" \ "unAuthPaymentsRuleChangeDate").json.put(JsString(date1)) and
                (__ \ "event11" \ "hasSchemeChangedRulesInvestmentsInAssets").json.put(JsBoolean(false))
              ).reduce
          case (_, Some(date2)) =>
            (
              (__ \ "event11" \ "hasSchemeChangedRulesUnAuthPayments").json.put(JsBoolean(false)) and
                (__ \ "event11" \ "hasSchemeChangedRulesInvestmentsInAssets").json.put(JsBoolean(true)) and
                (__ \ "event11" \ "investmentsInAssetsRuleChangeDate").json.put(JsString(date2))
              ).reduce
          case _ => Reads.pure(Json.obj())

        }

      }
    ) flatMap (identity)
  }


  private val event10Reads: Reads[JsObject] = {
    val event10ItemReads: Reads[JsObject] = {
      (
        (__ \ "invRegScheme" \ "startDateDetails" \ "startDateOfInvReg").readNullable[String] and
          (__ \ "invRegScheme" \ "startDateDetails" \ "contractsOrPolicies").readNullable[String] and
          (__ \ "invRegScheme" \ "ceaseDateDetails" \ "ceaseDateOfInvReg").readNullable[String]
        ) (
        (startDate, contractsOrPolicies, ceaseDate) => {
          (startDate, contractsOrPolicies, ceaseDate) match {
            case (Some(sd), Some(cop), None) =>
              (
                (__ \ "becomeOrCeaseScheme").json.put(JsString("itBecameAnInvestmentRegulatedPensionScheme")) and
                  (__ \ "schemeChangeDate" \ "schemeChangeDate").json.put(JsString(sd)) and
                  (__ \ "contractsOrPolicies").json.put(yesNoTransform(cop))
                ).reduce
            case (None, None, Some(cd)) =>
              (
                (__ \ "becomeOrCeaseScheme").json.put(JsString("itHasCeasedToBeAnInvestmentRegulatedPensionScheme")) and
                  (__ \ "schemeChangeDate" \ "schemeChangeDate").json.put(JsString(cd))
                ).reduce
            case (a, b, c) => Reads.failed[JsObject](s"Invalid $a $b $c")
          }
        }
      ).flatMap(identity)
    }
    (__ \ "eventDetails" \ "event10").read(Reads.seq(event10ItemReads)).map { s =>
      s.headOption match {
        case None => Json.obj()
        case Some(o) =>
          Json.obj(
            "event10" -> o
          )
      }
    }
  }

  private val yesNoTransform: String => JsBoolean = {
    case "Yes" => JsBoolean(true)
    case "No" => JsBoolean(false)
    case _ => JsBoolean(false)
  }
}
