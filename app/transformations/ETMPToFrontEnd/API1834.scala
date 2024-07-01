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
import transformations.Transformer
import scala.language.implicitConversions


object API1834 {
  import API1834ReadsUtilities._

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



}

private object API1834ReadsUtilities extends Transformer {
  import transformations.ETMPToFrontEnd.API1834Paths._

  val event18Reads: Reads[JsObject] = {
    (uaEvent18DetailsChargeablePmt.readNullable[String] and
      uaEvent18DetailsRecordVersion.readNullable[String])(
      (chargeablePmt, recordVersion) => {
        (chargeablePmt, recordVersion) match {
          case (Some(_), None) => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some("Yes"), Some(rv)) =>
            (uaEvent18Confirmation.json.put(JsBoolean(true)) and
              uaEvent18RecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)
  }

  val event19Reads: Reads[JsObject] = {
    val event19Node = (uaCountryCode.readNullable[String] and
      uaDateOfChange.readNullable[String] and
      uaRecordVersion.readNullable[String]
      )(
      (countryCode, dateOfChange, recordVersion) => {
        (countryCode, dateOfChange, recordVersion) match {
          case (_, _, None) => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some(code), Some(date), Some(rv)) => (
            uaDateChangeMade.json.put(JsString(date)) and
              uaCountryOrTerritory.json.put(JsString(code)) and
              uaRecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))
            ).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)

    uaEvent19Details.readNullable(Reads.seq(event19Node)).map {
      case None => Json.obj()
      case Some(dataOpt) =>
        dataOpt.headOption match {
          case Some(event19Data) => Json.obj("event19" -> event19Data)
          case None => Json.obj()
        }
    }
  }

  val event20Reads: Reads[JsObject] = {
    val event20Node = (uaStartDateOfOccScheme.readNullable[String] and
      uaStopDateOfOccScheme.readNullable[String] and
      uaRecordVersion.readNullable[String]
      )((optStartDate, optStopDate, recordVersion) => {
      (optStartDate, optStopDate, recordVersion) match {
        case (_, _, None) => Reads[JsObject](_ => JsError("record version is missing"))
        case (Some(startDate), None, Some(rv)) => (uaBecameDate.json.put(JsString(startDate)) and
          uaWhatChange.json.put(JsString("becameOccupationalScheme")) and
          uaRecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))
          ).reduce
        case (None, Some(stopDate), Some(rv)) => (uaCeasedDate.json.put(JsString(stopDate)) and
          uaWhatChange.json.put(JsString("ceasedOccupationalScheme")) and
          uaRecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))
          ).reduce
        case _ => Reads.pure(Json.obj())
      }
    }
    ).flatMap(identity)
    uaEvent20Details.readNullable(Reads.seq(event20Node)).map {
      case None => Json.obj()
      case Some(dataOpt) =>
        dataOpt.headOption match {
          case Some(event20Data) => Json.obj("event20" -> event20Data)
          case None => Json.obj()
        }
    }

  }

  val eventWindUpReads: Reads[JsObject] = {
    (uaWindUpDetailsRecordVersion.readNullable[String] and
      uaDateOfWindUp.readNullable[String]) (
      (recordVersion, dateOfWindUp) => {
        (recordVersion, dateOfWindUp) match {
          case (None, Some(_)) => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some(rv), Some(dateOfWindUp)) if dateOfWindUp != "9999-12-31" => ((__ \ "eventWindUp" \ "schemeWindUpDate").json.put(JsString(dateOfWindUp)) and
            uaWindUpRecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)
  }

  val event12Reads: Reads[JsObject] = {
    (uaEvent12DetailsRecordVersion.readNullable[String] and
      uaEvent12DetailsTwoOrMoreSchemesDate.readNullable[String])(
      (recordVersion, date) => (recordVersion, date) match {
        case (None, Some(_)) => Reads[JsObject](_ => JsError("record version is missing"))
        case (Some(rv), Some(d)) =>
          (uaEvent12HasSchemeChangedRules.json.put(JsBoolean(true)) and
            uaEvent12DateOfChange.json.put(JsString(d)) and
            uaEvent12RecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))).reduce
        case _ => Reads.pure(Json.obj())
      }
    ).flatMap(identity)
  }

  val event13Reads: Reads[JsObject] = {

    def mapStructure(s: String) = s match {
      case "A single trust under which all of the assets are held for the benefit of all members of the scheme" => "single"
      case "A group life/death in service scheme" => "group"
      case "A body corporate" => "corporate"
      case "Other" => "other"
    }

    val event13Node = (
      uaRecordVersion.readNullable[String] and
        uaDateOfChange.readNullable[String] and
        uaSchemeStructure.readNullable[String]
      )(
      (recordVersion, date, structure) => {
        (recordVersion, date, structure) match {
          case (None, _, _) => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some(rv), Some(d), Some(s)) =>
            (
              uaRecordVersion.json.put(JsNumber(rv.takeRight(3).toInt)) and
                uaSchemeStructure.json.put(JsString(mapStructure(s))) and
                uaChangeDate.json.put(JsString(d))
              ).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)

    uaEvent13.readNullable(Reads.seq(event13Node)).map {
      case None => Json.obj()
      case Some(dataOpt) =>
        dataOpt.headOption match {
          case Some(event13Data) => Json.obj("event13" -> event13Data)
          case None => Json.obj()
        }
    }
  }

  val event14Reads: Reads[JsObject] = {
    (uaEvent14DetailsSchemeMembers.readNullable[String] and
      uaEvent14DetailsRecordVersion.readNullable[String])(
      (schemeMembers, recordVersion) => {
        (schemeMembers, recordVersion) match {
          case (Some(_), None) => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some(data), Some(rv)) =>
            (uaEvent14SchemeMembers.json.put(JsString(data)) and
              uaEvent14RecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ).flatMap(identity)
  }

  val event11Reads: Reads[JsObject] = {
    (
      uaEvent11DetailsRecordVersion.readNullable[String] and
        uaEvent11DetailsUnauthorisedPmtsDate.readNullable[String] and
        uaEvent11DetailsContractsOrPoliciesDate.readNullable[String]
      )(
      (recordVersion, unAuthDate, contractsDate) => {
        (recordVersion, unAuthDate, contractsDate) match {
          case (None, a, b) if a.isDefined || b.isDefined => Reads[JsObject](_ => JsError("record version is missing"))
          case (Some(rv), Some(date1), Some(date2)) =>
            (
              uaEvent11HasSchemeChangedRulesUnAuthPayments.json.put(JsBoolean(true)) and
                uaEvent11UnAuthPaymentsRuleChangeDateValue.json.put(JsString(date1)) and
                uaEvent11HasSchemeChangedRulesInvestmentsInAssets.json.put(JsBoolean(true)) and
                uaEvent11InvestmentsInAssetsRuleChangeDateValue.json.put(JsString(date2)) and
                uaEvent11RecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))
              ).reduce
          case (Some(rv), Some(date1), _) =>
            (
              uaEvent11HasSchemeChangedRulesUnAuthPayments.json.put(JsBoolean(true)) and
                uaEvent11UnAuthPaymentsRuleChangeDate.json.put(JsString(date1)) and
                uaEvent11HasSchemeChangedRulesInvestmentsInAssets.json.put(JsBoolean(false)) and
                uaEvent11RecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))
              ).reduce
          case (Some(rv), _, Some(date2)) =>
            (
              uaEvent11HasSchemeChangedRulesUnAuthPayments.json.put(JsBoolean(false)) and
                uaEvent11HasSchemeChangedRulesInvestmentsInAssets.json.put(JsBoolean(true)) and
                uaEvent11InvestmentsInAssetsRuleChangeDate.json.put(JsString(date2)) and
                uaRecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))
              ).reduce
          case _ => Reads.pure(Json.obj())
        }
      }
    ) flatMap identity
  }


  val event10Reads: Reads[JsObject] = {
    val event10ItemReads: Reads[JsObject] = {
      (
        uaRecordVersion.readNullable[String] and
          uaStartDateOfInv.readNullable[String] and
          uaInvRegSchemeContractsOrPolicies.readNullable[String] and
          uaCeaseDateOfInvReg.readNullable[String]
        )(
        (recordVersion, startDate, contractsOrPolicies, ceaseDate) => {
          (recordVersion, startDate, contractsOrPolicies, ceaseDate) match {
            case (None, _, _, _) => Reads[JsObject](_ => JsError("record version is missing"))
            case (Some(rv), Some(sd), Some(cop), None) =>
              (
                uaBecomeOrCeaseScheme.json.put(JsString("itBecameAnInvestmentRegulatedPensionScheme")) and
                  uaSchemeChangeDate.json.put(JsString(sd)) and
                  uaContractsOrPolicies.json.put(yesNoTransformToJsBoolean(cop)) and
                  uaRecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))
                ).reduce
            case (Some(rv), None, None, Some(cd)) =>
              (
                uaBecomeOrCeaseScheme.json.put(JsString("itHasCeasedToBeAnInvestmentRegulatedPensionScheme")) and
                  uaSchemeChangeDate.json.put(JsString(cd)) and
                  uaRecordVersion.json.put(JsNumber(rv.takeRight(3).toInt))
                ).reduce
            case (_, a, b, c) => Reads.failed[JsObject](s"Invalid $a $b $c")
          }
        }
      ).flatMap(identity)
    }
    uaEvent10.readNullable(Reads.seq(event10ItemReads)).map {
      case None => Json.obj()
      case Some(dataOpt) =>
        dataOpt.headOption match {
          case None => Json.obj()
          case Some(event10Data) => Json.obj("event10" -> event10Data)
        }
    }
  }

  lazy val reqReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick)

  lazy val optReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick).orElse(doNothing)

}

private object API1834Paths {
  // UA
  val uaEvent10:                                                JsPath = __ \ "eventDetails" \ "event10"

  val uaEvent11:                                                JsPath = __ \ "event11"
  val uaEvent11Details:                                         JsPath = __ \ "eventDetails" \ "event11"
  val uaEvent11DetailsRecordVersion:                            JsPath = uaEvent11Details \ "recordVersion"
  val uaEvent11DetailsUnauthorisedPmtsDate:                     JsPath = uaEvent11Details \ "unauthorisedPmtsDate"
  val uaEvent11DetailsContractsOrPoliciesDate:                  JsPath = uaEvent11Details \ "contractsOrPoliciesDate"
  val uaEvent11RecordVersion:                                   JsPath = uaEvent11 \ "recordVersion"
  val uaEvent11HasSchemeChangedRulesUnAuthPayments:             JsPath = uaEvent11 \ "hasSchemeChangedRulesUnAuthPayments"
  val uaEvent11UnAuthPaymentsRuleChangeDate:                    JsPath = uaEvent11 \ "unAuthPaymentsRuleChangeDate"
  val uaEvent11UnAuthPaymentsRuleChangeDateValue:               JsPath = uaEvent11UnAuthPaymentsRuleChangeDate \ "date"
  val uaEvent11HasSchemeChangedRulesInvestmentsInAssets:        JsPath = uaEvent11 \ "hasSchemeChangedRulesInvestmentsInAssets"
  val uaEvent11InvestmentsInAssetsRuleChangeDate:               JsPath = uaEvent11 \ "investmentsInAssetsRuleChangeDate"
  val uaEvent11InvestmentsInAssetsRuleChangeDateValue:          JsPath = uaEvent11InvestmentsInAssetsRuleChangeDate \ "date"

  val uaEvent12:                                                JsPath = __ \ "event12"
  val uaEvent12Details:                                         JsPath = __ \ "eventDetails" \ "event12"
  val uaEvent12DetailsRecordVersion:                            JsPath = uaEvent12Details \ "recordVersion"
  val uaEvent12DetailsTwoOrMoreSchemesDate:                     JsPath = uaEvent12Details \ "twoOrMoreSchemesDate"
  val uaEvent12RecordVersion:                                   JsPath = uaEvent12 \ "recordVersion"
  val uaEvent12HasSchemeChangedRules:                           JsPath = uaEvent12 \ "hasSchemeChangedRules"
  val uaEvent12DateOfChange:                                    JsPath = uaEvent12 \ "dateOfChange" \ "dateOfChange"

  val uaEvent13:                                                JsPath = __ \ "eventDetails" \ "event13"

  val uaEvent14:                                                JsPath = __ \ "event14"
  val uaEvent14Details:                                         JsPath = __ \ "eventDetails" \ "event14"
  val uaEvent14DetailsSchemeMembers:                            JsPath = uaEvent14Details \ "schemeMembers"
  val uaEvent14DetailsRecordVersion:                            JsPath = uaEvent14Details \ "recordVersion"
  val uaEvent14SchemeMembers:                                   JsPath = uaEvent14 \ "schemeMembers"
  val uaEvent14RecordVersion:                                   JsPath = uaEvent14 \ "recordVersion"

  val uaEvent18:                                                JsPath = __ \ "event18"
  val uaEvent18Details:                                         JsPath = __ \ "eventDetails" \ "event18"
  val uaEvent18DetailsChargeablePmt:                            JsPath = uaEvent18Details \ "chargeablePmt"
  val uaEvent18DetailsRecordVersion:                            JsPath = uaEvent18Details \ "recordVersion"
  val uaEvent18Confirmation:                                    JsPath = uaEvent18 \ "event18Confirmation"
  val uaEvent18RecordVersion:                                   JsPath = uaEvent18 \ "recordVersion"

  val uaEvent19Details:                                         JsPath = __ \ "eventDetails" \ "event19"

  val uaEvent20Details:                                         JsPath = __ \ "eventDetails" \ "event20"

  val uaWindUp:                                                 JsPath = __ \ "eventWindUp"
  val uaWindUpDetails:                                          JsPath = __ \ "eventDetails" \ "eventWindUp"
  val uaWindUpDetailsRecordVersion:                             JsPath = uaWindUpDetails \ "recordVersion"
  val uaDateOfWindUp:                                           JsPath = uaWindUpDetails \ "dateOfWindUp"
  val uaWindUpRecordVersion:                                    JsPath = uaWindUp \ "recordVersion"

  val uaRecordVersion:                                          JsPath = __ \ "recordVersion"
  val uaStartDateOfInv:                                         JsPath = __ \ "invRegScheme" \ "startDateDetails" \ "startDateOfInvReg"
  val uaInvRegSchemeContractsOrPolicies:                        JsPath = __ \ "invRegScheme" \ "startDateDetails" \ "contractsOrPolicies"
  val uaCeaseDateOfInvReg:                                      JsPath = __ \ "invRegScheme" \ "ceaseDateDetails" \ "ceaseDateOfInvReg"
  val uaBecomeOrCeaseScheme:                                    JsPath = __ \ "becomeOrCeaseScheme"
  val uaSchemeChangeDate:                                       JsPath = __ \ "schemeChangeDate" \ "schemeChangeDate"
  val uaContractsOrPolicies:                                    JsPath = __ \ "contractsOrPolicies"
  val uaSchemeStructure:                                        JsPath = __ \ "schemeStructure"
  val uaChangeDate:                                             JsPath = __ \ "changeDate"
  val uaDateOfChange:                                           JsPath = __ \ "dateOfChange"
  val uaStartDateOfOccScheme:                                   JsPath = __ \ "occSchemeDetails" \ "startDateOfOccScheme"
  val uaStopDateOfOccScheme:                                    JsPath = __ \ "occSchemeDetails" \ "stopDateOfOccScheme"
  val uaBecameDate:                                             JsPath = __ \ "becameDate" \ "date"
  val uaCeasedDate:                                             JsPath = __ \ "ceasedDate" \ "date"
  val uaWhatChange:                                             JsPath = __ \ "whatChange"
  val uaCountryCode:                                            JsPath = __ \ "countryCode"
  val uaDateChangeMade:                                         JsPath = __ \ "dateChangeMade"
  val uaCountryOrTerritory:                                     JsPath = __ \ "CountryOrTerritory"
}
