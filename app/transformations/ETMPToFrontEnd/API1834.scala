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
    (__ \ "eventDetails" \ "event18" \ "chargeablePmt").readNullable[String].map {
      case Some("Yes") => (__ \ "event18" \ "event18Confirmation").json.put(JsBoolean(true))
      case _ => Reads.pure(Json.obj())
    }.flatMap(identity)
  }

  private val event19Reads = {
    val event19Node = ((__ \ "countryCode").readNullable[String] and
      (__ \ "dateOfChange").readNullable[String]
      ) (
      (countryCode, dateOfChange) => {
        (countryCode, dateOfChange) match {
          case (Some(code), Some(date)) => (
            (__ \ "dateChangeMade").json.put(JsString(date)) and
              (__ \ "CountryOrTerritory").json.put(JsString(code))
            ).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)

    (__ \ "eventDetails" \ "event19").read(Reads.seq(event19Node)).map{ dataOpt =>
      dataOpt.headOption match {
        case Some(event19Data) => Json.obj("event19" -> event19Data)
        case None => Json.obj()
      }
    }
  }

  private val event20Reads: Reads[JsObject] = {
    val event20Node = ((__ \ "occSchemeDetails"\ "startDateOfOccScheme").readNullable[String] and
      (__ \ "occSchemeDetails"\ "stopDateOfOccScheme").readNullable[String]
      )((optStartDate, optStopDate) => {
      (optStartDate, optStopDate) match {
        case (Some(startDate), None) => ((__ \ "becameDate"\ "date").json.put(JsString(startDate)) and
          (__ \ "whatChange").json.put(JsString("becameOccupationalScheme"))
          ).reduce
        case (None, Some(stopDate)) => ((__ \ "ceasedDate"\ "date").json.put(JsString(stopDate)) and
          (__ \ "whatChange").json.put(JsString("ceasedOccupationalScheme"))
          ).reduce
        case _ => Reads.pure(Json.obj())
      }
    }
    ).flatMap(identity)
    (__ \ "eventDetails" \ "event20").read(Reads.seq(event20Node)).map{ dataOpt =>
      dataOpt.headOption match {
        case Some(event20Data) => Json.obj("event20" -> event20Data)
        case None => Json.obj()
      }
    }

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

     val event13Node =  (
      (__ \ "dateOfChange").readNullable[String] and
        (__ \ "schemeStructure").readNullable[String]
      ) (
      (date, structure) => {
        (date, structure) match {
          case (Some(d), Some(s)) =>
            (
              (__ \ "schemeStructure").json.put(JsString(mapStructure(s))) and
                (__ \ "changeDate").json.put(JsString(d))
              ).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)

    (__ \ "eventDetails" \ "event13").read(Reads.seq(event13Node)).map { dataOpt =>
      dataOpt.headOption match {
        case Some(event13Data) => Json.obj("event13" -> event13Data)
        case None => Json.obj()
      }
    }
  }

  private val event14Reads = {
    (__ \ "eventDetails" \ "event14" \ "schemeMembers").readNullable[String].map{
      case Some(data) => (__ \ "event14" \ "schemeMembers").json.put(JsString(data))
      case _ => Reads.pure(Json.obj())
    }.flatMap(identity)
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
                (__ \ "event11" \ "unAuthPaymentsRuleChangeDate" \ "date").json.put(JsString(date1)) and
                (__ \ "event11" \ "hasSchemeChangedRulesInvestmentsInAssets").json.put(JsBoolean(true)) and
                (__ \ "event11" \ "investmentsInAssetsRuleChangeDate" \ "date").json.put(JsString(date2))
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
    ) flatMap identity
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
    (__ \ "eventDetails" \ "event10").read(Reads.seq(event10ItemReads)).map { dataOpt =>
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
