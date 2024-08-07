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

package transformations.UserAnswersToETMP

import models.enumeration.EventType
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.Reads.JsObjectReducer
import play.api.libs.json._
import transformations.{ReadsUtils, Transformer}

object API1826 {
  import transformations.UserAnswersToETMP.API1826ReadsUtilities._
  def transformToETMPData(deleteEvent: EventType, delete: Boolean): Reads[JsObject] = {

    def eventTypeNodes(events: Seq[JsObject]): JsObject = {
      val eventDetailNodes = events.foldLeft(Json.obj())((a, b) => a ++ b)
      if (events.isEmpty) Json.obj() else Json.obj("eventDetails" -> eventDetailNodes)
    }

    def deleteEventTransform(eventType: EventType, reads:Reads[Option[JsObject]]) = {
      if(delete && deleteEvent == eventType) {
        Reads.pure(None):Reads[Option[JsObject]]
      } else reads
    }

    for {
      ev10 <- deleteEventTransform(EventType.Event10,event10Reads)
      ev11 <- deleteEventTransform(EventType.Event11,event11Reads)
      ev12 <- deleteEventTransform(EventType.Event12,event12Reads)
      ev13 <- deleteEventTransform(EventType.Event13,event13Reads)
      ev14 <- deleteEventTransform(EventType.Event14,event14Reads)
      ev18 <- deleteEventTransform(EventType.Event18,event18Reads)
      ev19 <- deleteEventTransform(EventType.Event19,event19Reads)
      ev20 <- deleteEventTransform(EventType.Event20,event20Reads)
      schWindUp <- schemeWindUpReads(delete && deleteEvent == EventType.WindUp)
      header <- HeaderForAllAPIs.transformToETMPData()
    } yield {
      header ++ eventTypeNodes((ev10 ++ ev11 ++ ev12 ++ ev13 ++ ev14 ++ ev18 ++ ev19 ++ ev20 ++ schWindUp).toSeq)
    }
  }
}

private object API1826ReadsUtilities extends Transformer with ReadsUtils {
  import transformations.UserAnswersToETMP.API1826Paths._

  private def mapReadsToOptionArray(eventTypeNodeName: String)(reads: JsPath => Reads[JsObject]): Reads[Option[JsObject]] = {
    (__ \ eventTypeNodeName).readNullable[JsObject].flatMap {
      case Some(_) =>
        val uaBaseForEventType = __ \ eventTypeNodeName
        reads(uaBaseForEventType).flatMap(jsObject => (__ \ eventTypeNodeName).json.put(Json.arr(jsObject)).map(Option(_)))
      case _ =>
        Reads.pure(None)
    }
  }

  private val padLeftVersion: JsValue => JsString = {
    case JsNumber(s) => JsString(("00" + s).takeRight(3))
    case e => throw new RuntimeException("Quick fix" + e)
  }

  private def recordVersionReads(eventTypeNodeName: String): Reads[JsObject] =
    etmpPathToRecordVersion.json.copyFrom(uaPathToRecordVersion(eventTypeNodeName).json.pick.map(padLeftVersion))

  lazy val event10Reads: Reads[Option[JsObject]] = {
    mapReadsToOptionArray(eventTypeNodeName = "event10") { uaBaseForEventType =>
      (uaBaseForEventType \ "becomeOrCeaseScheme").read[String].flatMap {
        case "itBecameAnInvestmentRegulatedPensionScheme" =>
          (reqReads(etmpPathToStartDateOfInvReg, uaPathToSchemeChangeDate(uaBaseForEventType)) and
            etmpPathToContractsOrPolicies.json
              .copyFrom(uaPathToContractsOrPolicies(uaBaseForEventType).json.pick.map(toYesNo)) and
            recordVersionReads("event10")).reduce
        case _ =>
          (reqReads(etmpPathToCeaseDateOfInvReg, uaPathToSchemeChangeDate(uaBaseForEventType)) and
            recordVersionReads("event10")).reduce
      }
    }
  }

  lazy val event11Reads: Reads[Option[JsObject]] = {
    (
      uaEvent11.readNullable[JsObject] and
        uaRecordVersion("event11").readNullable[JsNumber]
      )(
      (et, version) =>
        (et, version) match {
          case (Some(json), Some(v)) =>

            val optReadsUnauthorisedPmtsDate = if ((json \ "hasSchemeChangedRulesUnAuthPayments").as[Boolean]) {
              (json \ "unAuthPaymentsRuleChangeDate" \ "date").asOpt[String]
            } else None
            val optReadsContractsOrPoliciesDate = if ((json \ "hasSchemeChangedRulesInvestmentsInAssets").as[Boolean]) {
              (json \ "investmentsInAssetsRuleChangeDate" \ "date").asOpt[String]
            } else None

            Some(

              (optReadsUnauthorisedPmtsDate, optReadsContractsOrPoliciesDate) match {
                case (Some(date1), Some(date2)) => Json.obj("event11" -> Json.obj(
                  "recordVersion" -> ("00" + v.value.toString).takeRight(3),
                  "unauthorisedPmtsDate" -> JsString(date1),
                  "contractsOrPoliciesDate" -> JsString(date2)))
                case (Some(date1), None) => Json.obj("event11" -> Json.obj(
                  "recordVersion" -> ("00" + v.value.toString).takeRight(3),
                  "unauthorisedPmtsDate" -> JsString(date1)))
                case (None, Some(date2)) => Json.obj("event11" -> Json.obj(
                  "recordVersion" -> ("00" + v.value.toString).takeRight(3),
                  "contractsOrPoliciesDate" -> JsString(date2)))
                case (None, None) => Json.obj() // Note: the FE prevents this option from being compiled.
              }
            )
          case _ => None
        }
    )
  }

  lazy val event12Reads: Reads[Option[JsObject]] = {
    (
      uaEvent12.readNullable[JsObject] and
        uaRecordVersion("event12").readNullable[JsNumber]
      )(
      (et, version) =>
        (et, version) match {
          case (Some(json), Some(v)) =>
            Some(
              Json.obj(
                "event12" ->
                  Json.obj(
                    "recordVersion" -> ("00" + v.value.toString).takeRight(3),
                    "twoOrMoreSchemesDate" -> (json \ "dateOfChange" \ "dateOfChange").as[String]
                  )
              )
            )
          case _ => None
        }
    )
  }

  private def event13SchemeStructureTransformer(schemeStructure: JsValue): JsString = {
    schemeStructure.as[JsString].value match {
      case "single" => JsString("A single trust under which all of the assets are held for the benefit of all members of the scheme")
      case "group" => JsString("A group life/death in service scheme")
      case "corporate" => JsString("A body corporate")
      case _ => JsString("Other")
    }
  }

  lazy val event13Reads: Reads[Option[JsObject]] = {
    mapReadsToOptionArray(eventTypeNodeName = "event13") { uaBaseForEventType =>
      (uaBaseForEventType \ "schemeStructure").read[String].flatMap {
        case "other" => (
          etmpPathToSchemeStructure.json.copyFrom(uaPathToSchemeStructure(uaBaseForEventType).json.pick.map(event13SchemeStructureTransformer)) and
            reqReads(etmpPathToDateOfChange, uaPathToChangeDate(uaBaseForEventType)) and
            optReads(etmpPathToSchemeStructureOther, uaPathToSchemeStructureDescription(uaBaseForEventType)) and
            recordVersionReads("event13")
          ).reduce
        case _ => (
          etmpPathToSchemeStructure.json.copyFrom(uaPathToSchemeStructure(uaBaseForEventType).json.pick.map(event13SchemeStructureTransformer)) and
            reqReads(etmpPathToDateOfChange, uaPathToChangeDate(uaBaseForEventType)) and
            recordVersionReads("event13")
          ).reduce
      }
    }
  }

  lazy val event14Reads: Reads[Option[JsObject]] = {
    (
      uaEvent14.readNullable[JsObject] and
        uaRecordVersion("event14").readNullable[JsNumber]
      )(
      (et, version) =>
        (et, version) match {
          case (Some(json), Some(v)) =>
            Some(
              Json.obj(
                "event14" -> Json.obj(
                  "recordVersion" -> ("00" + v.value.toString).takeRight(3),
                  "schemeMembers" -> (json \ "schemeMembers").as[String]
                )
              )
            )
          case _ => None
        }
    )
  }

  lazy val event18Reads: Reads[Option[JsObject]] = {
    (
      uaEvent18.readNullable[JsObject] and
        uaRecordVersion("event18").readNullable[JsNumber]
      )(
      (et, version) =>
        (et, version) match {
          case (Some(_), Some(v)) =>
            Some(Json.obj(
              "event18" -> Json.obj(
                "recordVersion" -> ("00" + v.value.toString).takeRight(3),
                "chargeablePmt" -> yes
              )
            )
            )
          case _ => None
        }
    )
  }

  lazy val event19Reads: Reads[Option[JsObject]] = {
    mapReadsToOptionArray(eventTypeNodeName = "event19") { uaBaseForEventType =>
      (reqReads(etmpPathToCountryCode, uaPathToCountryOrTerritory(uaBaseForEventType)) and
        reqReads(etmpPathToDateOfChange, uaPathToDateChangeMade(uaBaseForEventType)) and
        recordVersionReads("event19")
        ).reduce
    }
  }

  lazy val event20Reads: Reads[Option[JsObject]] = {
    mapReadsToOptionArray(eventTypeNodeName = "event20") { uaBaseForEventType =>
      (uaBaseForEventType \ "whatChange").read[String].flatMap {
        case "becameOccupationalScheme" =>
          (reqReads(etmpPathToStartDateOfOccScheme, uaPathToBecameDate(uaBaseForEventType)) and
            recordVersionReads("event20")).reduce
        case _ =>
          (reqReads(etmpPathToStopDateOfOccScheme, uaPathToCeasedDate(uaBaseForEventType)) and
            recordVersionReads("event20")).reduce
      }
    }
  }

  def schemeWindUpReads(delete: Boolean): Reads[Option[JsObject]] = {
    (
      uaEventWindUp.readNullable[JsObject] and
        uaRecordVersion("eventWindUp").readNullable[JsNumber]
      )(
      (et, version) =>
        (et, version) match {
          case (Some(json), Some(v)) =>
            Some(
              Json.obj(
                "eventWindUp" -> Json.obj(
                  "recordVersion" -> ("00" + v.value.toString).takeRight(3),
                  "dateOfWindUp" -> ( if(delete) "9999-12-31" else (json \ "schemeWindUpDate").as[String] )
                )
              )
            )
          case _ => None
        }
    )
  }

}

private object API1826Paths {
  // ETMP
  val etmpPathToCountryCode:                          JsPath = __ \ "countryCode"
  val etmpPathToDateOfChange:                         JsPath = __ \ "dateOfChange"
  val etmpPathToSchemeStructure:                      JsPath = __ \ "schemeStructure"
  val etmpPathToSchemeStructureOther:                 JsPath = __ \ "schemeStructureOther"
  val etmpPathToStartDateOfOccScheme:                 JsPath = __ \ "occSchemeDetails" \ "startDateOfOccScheme"
  val etmpPathToStopDateOfOccScheme:                  JsPath = __ \ "occSchemeDetails" \ "stopDateOfOccScheme"
  val etmpPathToRecordVersion:                        JsPath = __ \ "recordVersion"

  private val etmpPathToInvRegScheme:                 JsPath = __ \ "invRegScheme"
  val etmpPathToStartDateOfInvReg:                    JsPath = etmpPathToInvRegScheme \ "startDateDetails" \ "startDateOfInvReg"
  val etmpPathToContractsOrPolicies:                  JsPath = etmpPathToInvRegScheme \ "startDateDetails" \ "contractsOrPolicies"
  val etmpPathToCeaseDateOfInvReg:                    JsPath = etmpPathToInvRegScheme \ "ceaseDateDetails" \ "ceaseDateOfInvReg"

  // UA
  def uaPathToBecameDate(uaBaseForEventType: JsPath):                 JsPath = uaBaseForEventType \ "becameDate" \ "date"
  def uaPathToCeasedDate(uaBaseForEventType: JsPath):                 JsPath = uaBaseForEventType \ "ceasedDate" \ "date"
  def uaPathToChangeDate(uaBaseForEventType: JsPath):                 JsPath = uaBaseForEventType \ "changeDate"
  def uaPathToContractsOrPolicies(uaBaseForEventType: JsPath):        JsPath = uaBaseForEventType \ "contractsOrPolicies"
  def uaPathToCountryOrTerritory(uaBaseForEventType: JsPath):         JsPath = uaBaseForEventType \ "CountryOrTerritory"
  def uaPathToDateChangeMade(uaBaseForEventType: JsPath):             JsPath = uaBaseForEventType \ "dateChangeMade"
  def uaPathToSchemeChangeDate(uaBaseForEventType: JsPath):           JsPath = uaBaseForEventType \ "schemeChangeDate" \ "schemeChangeDate"
  def uaPathToSchemeStructure(uaBaseForEventType: JsPath):            JsPath = uaBaseForEventType \ "schemeStructure"
  def uaPathToSchemeStructureDescription(uaBaseForEventType: JsPath): JsPath = uaBaseForEventType \ "schemeStructureDescription"
  def uaPathToRecordVersion(eventTypeNodeName: String):               JsPath = __ \ eventTypeNodeName \ "recordVersion"

  def uaRecordVersion(eventType: String):                             JsPath = __ \ eventType \ "recordVersion"

  val uaEvent11:                                                      JsPath = __ \ "event11"

  val uaEvent12:                                                      JsPath = __ \ "event12"

  val uaEvent14:                                                      JsPath = __ \ "event14"

  val uaEvent18:                                                      JsPath = __ \ "event18"

  val uaEventWindUp:                                                  JsPath = __ \ "eventWindUp"
}
