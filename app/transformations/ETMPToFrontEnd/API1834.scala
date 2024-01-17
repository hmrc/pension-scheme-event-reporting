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

package transformations.ETMPToFrontEnd

import models.enumeration.EventType
import models.enumeration.EventType.{Event10, Event11, Event12, Event13, Event14, Event18, Event19, Event20, WindUp}
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
      case Event14 => event14Reads
      case Event18 => event18Reads
      case Event19 => event19Reads
      case Event20 => event20Reads
      case WindUp => eventWindUpReads
      case e => Reads.failed(s"Unknown event type $e")
    }
  }

  private val event18Reads = {
    ((__ \ "eventDetails" \ "event18" \ "chargeablePmt").readNullable[String] and
    (__ \ "eventDetails" \ "event18" \ "recordVersion").readNullable[String])(
      (chargeablePmt, recordVersion) => {
        (chargeablePmt, recordVersion) match {
          case (Some(_), None) => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some("Yes"), Some(rv)) =>
            ((__ \ "event18" \ "event18Confirmation").json.put(JsBoolean(true)) and
              (__ \ "event18" \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)
  }

  private val event19Reads = {
    val event19Node = ((__ \ "countryCode").readNullable[String] and
      (__ \ "dateOfChange").readNullable[String] and
      (__ \ "recordVersion").readNullable[String]
      )(
      (countryCode, dateOfChange, recordVersion) => {
        (countryCode, dateOfChange, recordVersion) match {
          case (_, _, None) => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some(code), Some(date), Some(rv)) => (
            (__ \ "dateChangeMade").json.put(JsString(date)) and
              (__ \ "CountryOrTerritory").json.put(JsString(code)) and
              (__ \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))
            ).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)

    (__ \ "eventDetails" \ "event19").readNullable(Reads.seq(event19Node)).map {
      case None => Json.obj()
      case Some(dataOpt) =>
        dataOpt.headOption match {
          case Some(event19Data) => Json.obj("event19" -> event19Data)
          case None => Json.obj()
        }
    }
  }

  private val event20Reads: Reads[JsObject] = {
    val event20Node = ((__ \ "occSchemeDetails" \ "startDateOfOccScheme").readNullable[String] and
      (__ \ "occSchemeDetails" \ "stopDateOfOccScheme").readNullable[String] and
      (__ \ "recordVersion").readNullable[String]
      )((optStartDate, optStopDate, recordVersion) => {
      (optStartDate, optStopDate, recordVersion) match {
        case (_, _, None) => Reads[JsObject](_ => JsError("record version is missing"))
        case (Some(startDate), None, Some(rv)) => ((__ \ "becameDate" \ "date").json.put(JsString(startDate)) and
          (__ \ "whatChange").json.put(JsString("becameOccupationalScheme")) and
          (__ \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))
          ).reduce
        case (None, Some(stopDate), Some(rv)) => ((__ \ "ceasedDate" \ "date").json.put(JsString(stopDate)) and
          (__ \ "whatChange").json.put(JsString("ceasedOccupationalScheme")) and
          (__ \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))
          ).reduce
        case _ => Reads.pure(Json.obj())
      }
    }
    ).flatMap(identity)
    (__ \ "eventDetails" \ "event20").readNullable(Reads.seq(event20Node)).map {
      case None => Json.obj()
      case Some(dataOpt) =>
        dataOpt.headOption match {
          case Some(event20Data) => Json.obj("event20" -> event20Data)
          case None => Json.obj()
        }
    }

  }

  private val eventWindUpReads: Reads[JsObject] = {
    ((__ \ "eventDetails" \ "eventWindUp" \ "recordVersion").readNullable[String] and
      (__ \ "eventDetails" \ "eventWindUp" \ "dateOfWindUp").readNullable[String]) (
      (recordVersion, dateOfWindUp) => {
        (recordVersion, dateOfWindUp) match {
          case (None, Some(_)) => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some(rv), Some(dateOfWindUp)) if dateOfWindUp != "9999-12-31" => ((__ \ "eventWindUp" \ "schemeWindUpDate").json.put(JsString(dateOfWindUp)) and
            (__ \ "eventWindUp" \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)
  }

  private val event12Reads: Reads[JsObject] = {
    ((__ \ "eventDetails" \ "event12" \ "recordVersion").readNullable[String] and
      (__ \ "eventDetails" \ "event12" \ "twoOrMoreSchemesDate").readNullable[String])(
      (recordVersion, date) => (recordVersion, date) match {
        case (None, Some(_)) => Reads[JsObject](_ => JsError("record version is missing"))
        case (Some(rv), Some(d)) =>
          ((__ \ "event12" \ "hasSchemeChangedRules").json.put(JsBoolean(true)) and
            (__ \ "event12" \ "dateOfChange" \ "dateOfChange").json.put(JsString(d)) and
            (__ \ "event12" \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))).reduce
        case _ => Reads.pure(Json.obj())
      }
    ).flatMap(identity)
  }

  private val event13Reads: Reads[JsObject] = {

    def mapStructure(s: String) = s match {
      case "A single trust under which all of the assets are held for the benefit of all members of the scheme" => "single"
      case "A group life/death in service scheme" => "group"
      case "A body corporate" => "corporate"
      case "Other" => "other"
    }

    val event13Node = (
      (__ \ "recordVersion").readNullable[String] and
      (__ \ "dateOfChange").readNullable[String] and
        (__ \ "schemeStructure").readNullable[String]
      )(
      (recordVersion, date, structure) => {
        (recordVersion, date, structure) match {
          case (None, _, _) => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some(rv), Some(d), Some(s)) =>
            (
              (__ \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt)) and
              (__ \ "schemeStructure").json.put(JsString(mapStructure(s))) and
                (__ \ "changeDate").json.put(JsString(d))
              ).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)

    (__ \ "eventDetails" \ "event13").readNullable(Reads.seq(event13Node)).map {
      case None => Json.obj()
      case Some(dataOpt) =>
        dataOpt.headOption match {
          case Some(event13Data) => Json.obj("event13" -> event13Data)
          case None => Json.obj()
        }
    }
  }

  private val event14Reads = {
    ((__ \ "eventDetails" \ "event14" \ "schemeMembers").readNullable[String] and
      (__ \ "eventDetails" \ "event14" \ "recordVersion").readNullable[String])(
      (schemeMembers, recordVersion) => {
        (schemeMembers, recordVersion) match {
          case (Some(_), None) => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some(data), Some(rv)) =>
            ((__ \ "event14" \ "schemeMembers").json.put(JsString(data)) and
              (__ \ "event14" \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)
  }

  private val event11Reads: Reads[JsObject] = {
    (
      (__ \ "eventDetails" \ "event11" \ "recordVersion").readNullable[String] and
        (__ \ "eventDetails" \ "event11" \ "unauthorisedPmtsDate").readNullable[String] and
        (__ \ "eventDetails" \ "event11" \ "contractsOrPoliciesDate").readNullable[String]
      )(
      (recordVersion, unAuthDate, contractsDate) => {
        (recordVersion, unAuthDate, contractsDate) match {
          case (None, a, b) if a.isDefined || b.isDefined => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some(rv), Some(date1), Some(date2)) =>
            (
              (__ \ "event11" \ "hasSchemeChangedRulesUnAuthPayments").json.put(JsBoolean(true)) and
                (__ \ "event11" \ "unAuthPaymentsRuleChangeDate" \ "date").json.put(JsString(date1)) and
                (__ \ "event11" \ "hasSchemeChangedRulesInvestmentsInAssets").json.put(JsBoolean(true)) and
                (__ \ "event11" \ "investmentsInAssetsRuleChangeDate" \ "date").json.put(JsString(date2)) and
                (__ \ "event11" \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))
              ).reduce
          case (Some(rv), Some(date1), _) =>
            (
              (__ \ "event11" \ "hasSchemeChangedRulesUnAuthPayments").json.put(JsBoolean(true)) and
                (__ \ "event11" \ "unAuthPaymentsRuleChangeDate").json.put(JsString(date1)) and
                (__ \ "event11" \ "hasSchemeChangedRulesInvestmentsInAssets").json.put(JsBoolean(false)) and
                (__ \ "event11" \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))
              ).reduce
          case (Some(rv), _, Some(date2)) =>
            (
              (__ \ "event11" \ "hasSchemeChangedRulesUnAuthPayments").json.put(JsBoolean(false)) and
                (__ \ "event11" \ "hasSchemeChangedRulesInvestmentsInAssets").json.put(JsBoolean(true)) and
                (__ \ "event11" \ "investmentsInAssetsRuleChangeDate").json.put(JsString(date2)) and
                (__ \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))
              ).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ) flatMap identity
  }


  private val event10Reads: Reads[JsObject] = {
    val event10ItemReads: Reads[JsObject] = {
      (
        (__ \ "recordVersion").readNullable[String] and
          (__ \ "invRegScheme" \ "startDateDetails" \ "startDateOfInvReg").readNullable[String] and
          (__ \ "invRegScheme" \ "startDateDetails" \ "contractsOrPolicies").readNullable[String] and
          (__ \ "invRegScheme" \ "ceaseDateDetails" \ "ceaseDateOfInvReg").readNullable[String]
        )(
        (recordVersion, startDate, contractsOrPolicies, ceaseDate) => {
          (recordVersion, startDate, contractsOrPolicies, ceaseDate) match {
            case (None, _, _, _) => Reads[JsObject](_ => JsError("record version is missing"))
            case (Some(rv), Some(sd), Some(cop), None) =>
              (
                (__ \ "becomeOrCeaseScheme").json.put(JsString("itBecameAnInvestmentRegulatedPensionScheme")) and
                  (__ \ "schemeChangeDate" \ "schemeChangeDate").json.put(JsString(sd)) and
                  (__ \ "contractsOrPolicies").json.put(yesNoTransform(cop)) and
                  (__ \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))
                ).reduce
            case (Some(rv), None, None, Some(cd)) =>
              (
                (__ \ "becomeOrCeaseScheme").json.put(JsString("itHasCeasedToBeAnInvestmentRegulatedPensionScheme")) and
                  (__ \ "schemeChangeDate" \ "schemeChangeDate").json.put(JsString(cd)) and
                  (__ \ "recordVersion").json.put(JsNumber(rv.takeRight(3).toInt))
                ).reduce
            case (_, a, b, c) => Reads.failed[JsObject](s"Invalid $a $b $c")
          }
        }
      ).flatMap(identity)
    }
    (__ \ "eventDetails" \ "event10").readNullable(Reads.seq(event10ItemReads)).map {
      case None => Json.obj()
      case Some(dataOpt) =>
        dataOpt.headOption match {
          case None => Json.obj()
          case Some(event10Data) => Json.obj("event10" -> event10Data)
        }
    }
  }

  private val yesNoTransform: String => JsBoolean = {
    case "Yes" => JsBoolean(true)
    case "No" => JsBoolean(false)
    case _ => JsBoolean(false)
  }
}
