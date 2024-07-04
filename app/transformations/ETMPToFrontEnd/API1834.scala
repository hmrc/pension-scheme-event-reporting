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

  private def readsRecordVersion(recordVersionPath: JsPath): Reads[JsNumber] = {
    recordVersionPath.json.pick.flatMap {
      case JsString(rv) => Reads.pure(JsNumber(rv.takeRight(3).toInt))
      case _ => fail(JsNumber(0))
    }
  }

  private val event10ReadsBecame: Reads[JsObject] = {
    (reqNestedReadsJsString(uaEvent10BecomeOrCeaseScheme, Reads.pure(JsString("itBecameAnInvestmentRegulatedPensionScheme"))) and
      reqReadsBoolTransform(uaEvent10ContractsOrPolicies, etmpEvent10ContractsOrPolicies, yesNoTransformToBoolean) and
      reqReads(uaEvent10SchemeChangeDate, etmpEvent10StartDateOfInvReg)).reduce
  }

  private val event10ReadsCeased: Reads[JsObject] = {
    (reqNestedReadsJsString(uaEvent10BecomeOrCeaseScheme, Reads.pure(JsString("itHasCeasedToBeAnInvestmentRegulatedPensionScheme"))) and
      reqReads(uaEvent10SchemeChangeDate, etmpEvent10CeaseDateOfInvReg)).reduce
  }

  private val readsEvent10BecameOrCeased: Reads[JsObject] = event10ReadsBecame.orElse(event10ReadsCeased)

  val event10Reads: Reads[JsObject] = {
    (reqNestedReadsJsNumber(uaRecordVersion("event10"), readsRecordVersion(etmpEvent10RecordVersion)) and
      readsEvent10BecameOrCeased).reduce
  }.orElse(Reads.pure(Json.obj()))

  private val readsUnAuthPaymentsRuleChanged: Reads[JsObject] = {
    etmpEvent11UnauthorisedPmtsDate.json.pick.flatMap {
      case JsString(_) =>
        (reqNestedReadsJsBoolean(uaEvent11HasSchemeChangedRulesUnAuthPayments, Reads.pure(JsBoolean(true))) and
          reqReads(uaEvent11UnAuthPaymentsRuleChangeDateValue, etmpEvent11UnauthorisedPmtsDate)).reduce
      case _ => reqNestedReadsJsBoolean(uaEvent11HasSchemeChangedRulesUnAuthPayments, Reads.pure(JsBoolean(false)))
    }
  }

  private val readsInvestmentsInAssetsRuleChange: Reads[JsObject] = {
    etmpEvent11ContractsOrPoliciesDate.json.pick.flatMap {
      case JsString(_) =>
        (reqNestedReadsJsBoolean(uaEvent11HasSchemeChangedRulesInvestmentsInAssets, Reads.pure(JsBoolean(true))) and
        reqReads(uaEvent11InvestmentsInAssetsRuleChangeDateValue, etmpEvent11ContractsOrPoliciesDate)).reduce
      case _ => reqNestedReadsJsBoolean(uaEvent11HasSchemeChangedRulesInvestmentsInAssets, Reads.pure(JsBoolean(false)))
    }
  }

  val event11Reads: Reads[JsObject] = {
    (reqNestedReadsJsNumber(uaRecordVersion("event11"), readsRecordVersion(etmpRecordVersion("event11"))) and
      readsUnAuthPaymentsRuleChanged and
      readsInvestmentsInAssetsRuleChange).reduce
  }.orElse(Reads.pure(Json.obj()))

  val event12Reads: Reads[JsObject] = {
    (reqNestedReadsJsNumber(uaRecordVersion("event12"), readsRecordVersion(etmpRecordVersion("event12"))) and
      reqReads(uaEvent12DateOfChange, etmpEvent12TwoOrMoreSchemesDate) and
      reqNestedReadsJsBoolean(uaEvent12HasSchemeChangedRules, Reads.pure(JsBoolean(true)))).reduce
  }.orElse(Reads.pure(Json.obj()))


  private val readsEvent13SchemeStructure = {
        def mapStructure(s: String) = s match {
          case "A single trust under which all of the assets are held for the benefit of all members of the scheme" => "single"
          case "A group life/death in service scheme" => "group"
          case "A body corporate" => "corporate"
          case "Other" => "other"
        }

    etmpEvent13SchemeStructure.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(mapStructure(str)))
      case _ => fail(JsString("event 13 scheme structure not found"))
    }
  }

  val event13Reads: Reads[JsObject] = {
    (reqNestedReadsJsNumber(uaRecordVersion("event13"), readsRecordVersion(etmpEvent13RecordVersion)) and
    reqNestedReadsJsString(uaEvent13SchemeStructure, readsEvent13SchemeStructure) and
      reqReads(uaEvent13ChangeDate, etmpEvent13DateOfChange)).reduce
  }.orElse(Reads.pure(Json.obj()))

  val event14Reads: Reads[JsObject] = {
    (reqNestedReadsJsNumber(uaRecordVersion("event14"), readsRecordVersion(etmpRecordVersion("event14"))) and
      reqReads(uaEvent14SchemeMembers, etmpEvent14SchemeMembers)).reduce
  }.orElse(Reads.pure(Json.obj()))

  val event18Reads: Reads[JsObject] = {
    (reqNestedReadsJsNumber(uaRecordVersion("event18"), readsRecordVersion(etmpRecordVersion("event18"))) and
      reqReadsBoolTransform(uaEvent18Confirmation,
        etmpEvent18ChargeablePmt, yesNoTransformToBoolean)).reduce
  }.orElse(Reads.pure(Json.obj()))

  val event19Reads: Reads[JsObject] = {
    (reqNestedReadsJsNumber(uaRecordVersion("event19"), readsRecordVersion(etmpEvent19RecordVersion)) and
      reqReads(uaEvent19CountryOrTerritory, etmpEvent19CountryCode) and
      reqReads(uaEvent19DateChangeMade, etmpEvent19DateOfChange)).reduce
  }.orElse(Reads.pure(Json.obj()))

  private val readsEvent20Became = {
    (reqReads(uaEvent20BecameDate, etmpEvent20StartDateOfOccScheme) and
      reqNestedReadsJsString(uaEvent20WhatChange, Reads.pure(JsString("becameOccupationalScheme")))
      ).reduce
  }

  private val readsEvent20Ceased = {
    (reqReads(uaEvent20CeasedDate, etmpEvent20StopDateOfOccScheme) and
      reqNestedReadsJsString(uaEvent20WhatChange, Reads.pure(JsString("ceasedOccupationalScheme")))
      ).reduce
  }

  val event20Reads: Reads[JsObject] = {
    (reqNestedReadsJsNumber(uaRecordVersion("event20"), readsRecordVersion(etmpEvent20RecordVersion)) and
      readsEvent20Became.orElse(readsEvent20Ceased)).reduce
  }.orElse(Reads.pure(Json.obj()))

  val eventWindUpReads: Reads[JsObject] = {
    (reqNestedReadsJsNumber(uaRecordVersion("eventWindUp"), readsRecordVersion(etmpRecordVersion("eventWindUp"))) and
      reqReads(uaWindUpDateOfWindUp, etmpEventWindUpDateOfWindUp)).reduce
  }.orElse(Reads.pure(Json.obj()))

  lazy val reqReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick)

  private lazy val reqNestedReadsJsNumber: (JsPath, Reads[JsNumber]) => Reads[JsObject] =
    (uaPath: JsPath, etmpReads: Reads[JsNumber]) => uaPath.json.copyFrom(etmpReads)

  private lazy val reqNestedReadsJsBoolean: (JsPath, Reads[JsBoolean]) => Reads[JsObject] =
    (uaPath: JsPath, etmpReads: Reads[JsBoolean]) => uaPath.json.copyFrom(etmpReads)

  private lazy val reqReadsBoolTransform: (JsPath, JsPath, String => Boolean) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => Boolean) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsBoolean(transform(str)))
        case _ => fail[JsBoolean]
      })
    }

  private lazy val reqNestedReadsJsString: (JsPath, Reads[JsString]) => Reads[JsObject] =
    (uaPath: JsPath, etmpReads: Reads[JsString]) => uaPath.json.copyFrom(etmpReads)

  lazy val optReads: (JsPath, JsPath) => Reads[JsObject] = (uaPath: JsPath, etmpPath: JsPath) => uaPath.json.copyFrom(etmpPath.json.pick).orElse(doNothing)

}

private object API1834Paths {
  // UA
  def uaRecordVersion(eventType: String):                       JsPath = __ \ eventType \ "recordVersion"

  private val uaEvent10:                                        JsPath = __ \ "event10"
  val uaEvent10BecomeOrCeaseScheme:                             JsPath = uaEvent10 \ "becomeOrCeaseScheme"
  val uaEvent10ContractsOrPolicies:                             JsPath = uaEvent10 \ "contractsOrPolicies"
  val uaEvent10SchemeChangeDate:                                JsPath = uaEvent10 \ "schemeChangeDate" \ "schemeChangeDate"

  private val uaEvent11:                                        JsPath = __ \ "event11"
  val uaEvent11HasSchemeChangedRulesUnAuthPayments:             JsPath = uaEvent11 \ "hasSchemeChangedRulesUnAuthPayments"
  private val uaEvent11UnAuthPaymentsRuleChangeDate:            JsPath = uaEvent11 \ "unAuthPaymentsRuleChangeDate"
  val uaEvent11HasSchemeChangedRulesInvestmentsInAssets:        JsPath = uaEvent11 \ "hasSchemeChangedRulesInvestmentsInAssets"
  val uaEvent11UnAuthPaymentsRuleChangeDateValue:               JsPath = uaEvent11UnAuthPaymentsRuleChangeDate \ "date"
  private val uaEvent11InvestmentsInAssetsRuleChangeDate:       JsPath = uaEvent11 \ "investmentsInAssetsRuleChangeDate"
  val uaEvent11InvestmentsInAssetsRuleChangeDateValue:          JsPath = uaEvent11InvestmentsInAssetsRuleChangeDate \ "date"

  private val uaEvent12:                                        JsPath = __ \ "event12"
  val uaEvent12DateOfChange:                                    JsPath = uaEvent12 \ "dateOfChange" \ "dateOfChange"
  val uaEvent12HasSchemeChangedRules:                           JsPath = uaEvent12 \ "hasSchemeChangedRules"

  private val uaEvent13:                                        JsPath = __ \ "event13"
  val uaEvent13ChangeDate:                                      JsPath = uaEvent13 \ "changeDate"
  val uaEvent13SchemeStructure:                                 JsPath = uaEvent13 \ "schemeStructure"

  private val uaEvent14:                                        JsPath = __ \ "event14"
  val uaEvent14SchemeMembers:                                   JsPath = uaEvent14 \ "schemeMembers"

  private val uaEvent18:                                        JsPath = __ \ "event18"
  val uaEvent18Confirmation:                                    JsPath = uaEvent18 \ "event18Confirmation"

  private val uaEvent19:                                        JsPath = __ \ "event19"
  val uaEvent19CountryOrTerritory:                              JsPath = uaEvent19 \ "CountryOrTerritory"
  val uaEvent19DateChangeMade:                                  JsPath = uaEvent19 \ "dateChangeMade"

  private val uaEvent20:                                        JsPath = __ \ "event20"
  val uaEvent20BecameDate:                                      JsPath = uaEvent20 \ "becameDate" \ "date"
  val uaEvent20CeasedDate:                                      JsPath = uaEvent20 \ "ceasedDate" \ "date"
  val uaEvent20WhatChange:                                      JsPath = uaEvent20 \ "whatChange"

  private val uaWindUp:                                         JsPath = __ \ "eventWindUp"
  val uaWindUpDateOfWindUp:                                     JsPath = uaWindUp \ "schemeWindUpDate"

  // ETMP
  private val etmpEventDetails:                                 JsPath = __ \ "eventDetails"

  def etmpRecordVersion(eventType: String):                     JsPath = etmpEventDetails \ eventType \ "recordVersion"

  private val etmpEvent10:                                      JsPath = etmpEventDetails \ "event10"
  val etmpEvent10RecordVersion:                                 JsPath = etmpEvent10 \ 0 \ "recordVersion"
  private val etmpEvent10InvRegScheme:                          JsPath = etmpEvent10 \ 0 \ "invRegScheme"
  private val etmpEvent10StartDateDetails:                      JsPath = etmpEvent10InvRegScheme \ "startDateDetails"
  val etmpEvent10ContractsOrPolicies:                           JsPath = etmpEvent10StartDateDetails \ "contractsOrPolicies"
  val etmpEvent10StartDateOfInvReg:                             JsPath = etmpEvent10StartDateDetails \ "startDateOfInvReg"
  private val etmpEvent10CeaseDateDetails:                      JsPath = etmpEvent10InvRegScheme \ "ceaseDateDetails"
  val etmpEvent10CeaseDateOfInvReg:                             JsPath = etmpEvent10CeaseDateDetails \ "ceaseDateOfInvReg"

  private val etmpEvent11:                                      JsPath = etmpEventDetails \ "event11"
  val etmpEvent11ContractsOrPoliciesDate:                       JsPath = etmpEvent11 \ "contractsOrPoliciesDate"
  val etmpEvent11UnauthorisedPmtsDate:                          JsPath = etmpEvent11 \ "unauthorisedPmtsDate"

  private val etmpEvent12:                                      JsPath = etmpEventDetails \ "event12"
  val etmpEvent12TwoOrMoreSchemesDate:                          JsPath = etmpEvent12 \ "twoOrMoreSchemesDate"

  private val etmpEvent13:                                      JsPath = etmpEventDetails \ "event13"
  val etmpEvent13DateOfChange:                                  JsPath = etmpEvent13 \ 0 \ "dateOfChange"
  val etmpEvent13RecordVersion:                                 JsPath = etmpEvent13 \ 0 \ "recordVersion"
  val etmpEvent13SchemeStructure:                               JsPath = etmpEvent13 \ 0 \ "schemeStructure"

  private val etmpEvent14:                                      JsPath = etmpEventDetails \ "event14"
  val etmpEvent14SchemeMembers:                                 JsPath = etmpEvent14 \ "schemeMembers"

  private val etmpEvent18:                                      JsPath = etmpEventDetails \ "event18"
  val etmpEvent18ChargeablePmt:                                 JsPath = etmpEvent18 \ "chargeablePmt"

  private val etmpEvent19:                                      JsPath = etmpEventDetails \ "event19"
  val etmpEvent19CountryCode:                                   JsPath = etmpEvent19 \ 0 \ "countryCode"
  val etmpEvent19DateOfChange:                                  JsPath = etmpEvent19 \ 0 \ "dateOfChange"
  val etmpEvent19RecordVersion:                                 JsPath = etmpEvent19 \ 0 \ "recordVersion"

  private val etmpEvent20:                                      JsPath = etmpEventDetails \ "event20"
  val etmpEvent20RecordVersion:                                 JsPath = etmpEvent20 \ 0 \ "recordVersion"
  private val etmpOccSchemeDetails:                             JsPath = etmpEvent20 \ 0 \ "occSchemeDetails"
  val etmpEvent20StartDateOfOccScheme:                          JsPath = etmpOccSchemeDetails \ "startDateOfOccScheme"
  val etmpEvent20StopDateOfOccScheme:                           JsPath = etmpOccSchemeDetails \ "stopDateOfOccScheme"

  private val etmpEventWindUp:                                  JsPath = etmpEventDetails \ "eventWindUp"
  val etmpEventWindUpDateOfWindUp:                              JsPath = etmpEventWindUp \ "dateOfWindUp"
}
