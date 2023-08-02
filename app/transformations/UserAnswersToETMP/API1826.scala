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

package transformations.UserAnswersToETMP

import models.enumeration.EventType
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.Reads.JsObjectReducer
import play.api.libs.json._
import transformations.Transformer

object API1826 extends Transformer {

  private def optField(fieldName: String, value: Option[String]) = {
    value.map(fieldName -> JsString(_))
  }

  private def optObj[T](objName: String, wrapper: String => T, value: Option[String]) = {
    value.map(objName -> wrapper(_))
  }

  private def mapReadsToOptionArray(eventTypeNodeName: String)(reads: JsPath => Reads[JsObject]): Reads[Option[JsObject]] = {
    (__ \ eventTypeNodeName).readNullable[JsObject].flatMap {
      case Some(_) =>
        val uaBaseForEventType = __ \ eventTypeNodeName
        reads(uaBaseForEventType).flatMap(jsObject => (__ \ eventTypeNodeName).json.put(Json.arr(jsObject)).map(Option(_)))
      case _ =>
        Reads.pure(None)
    }
  }

  //TODO: The below method is to be used for the refactor ticket PODS-8410
  private def mapReadsToOptionObject(eventTypeNodeName: String)(reads: JsPath => Reads[JsObject]): Reads[Option[JsObject]] = {
    (__ \ eventTypeNodeName).readNullable[JsObject].flatMap {
      case Some(_) =>
        val uaBaseForEventType = __ \ eventTypeNodeName
        reads(uaBaseForEventType).flatMap(jsObject => (__ \ eventTypeNodeName).json.put(jsObject)).map(Option(_))
      case _ =>
        Reads.pure(None)
    }
  }

  private val padLeftVersion: JsValue => JsString = {
    case JsNumber(s) => JsString(("00" + s).takeRight(3))
    case e => throw new RuntimeException("Quick fix" + e)
  }

  private def recordVersionReads(eventTypeNodeName: String): Reads[JsObject] =
    (__ \ "recordVersion").json.copyFrom((__ \ eventTypeNodeName \ "recordVersion").json.pick.map(padLeftVersion))

  private val invRegScheme = "invRegScheme"

  private lazy val event10Reads: Reads[Option[JsObject]] = {
    mapReadsToOptionArray(eventTypeNodeName = "event10") { uaBaseForEventType =>
      (uaBaseForEventType \ "becomeOrCeaseScheme").read[String].flatMap {
        case "itBecameAnInvestmentRegulatedPensionScheme" =>
          ((__ \ invRegScheme \ "startDateDetails" \ "startDateOfInvReg").json
            .copyFrom((uaBaseForEventType \ "schemeChangeDate" \ "schemeChangeDate").json.pick) and
            (__ \ invRegScheme \ "startDateDetails" \ "contractsOrPolicies").json
              .copyFrom((uaBaseForEventType \ "contractsOrPolicies").json.pick.map(toYesNo)) and
            recordVersionReads("event10")).reduce
        case _ =>
          ((__ \ invRegScheme \ "ceaseDateDetails" \ "ceaseDateOfInvReg").json
            .copyFrom((uaBaseForEventType \ "schemeChangeDate" \ "schemeChangeDate").json.pick) and
            recordVersionReads("event10")).reduce
      }
    }
  }


  private lazy val event11Reads = {
    (
      (__ \ "event11").readNullable[JsObject] and
        (__ \ "event11" \ "recordVersion").readNullable[JsNumber]
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

  private lazy val event12Reads = {
    (
      (__ \ "event12").readNullable[JsObject] and
        (__ \ "event12" \ "recordVersion").readNullable[JsNumber]
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

  private lazy val event13Reads: Reads[Option[JsObject]] = {
    mapReadsToOptionArray(eventTypeNodeName = "event13") { uaBaseForEventType =>
      (uaBaseForEventType \ "schemeStructure").read[String].flatMap {
        case "other" => (
          (__ \ "schemeStructure").json.copyFrom((uaBaseForEventType \ "schemeStructure").json.pick.map(event13SchemeStructureTransformer)) and
            (__ \ "dateOfChange").json.copyFrom((uaBaseForEventType \ "changeDate").json.pick) and
            ((__ \ "schemeStructureOther").json.copyFrom((uaBaseForEventType \ "schemeStructureDescription").json.pick) orElse doNothing) and
            recordVersionReads("event13")
          ).reduce
        case _ => (
          (__ \ "schemeStructure").json.copyFrom((uaBaseForEventType \ "schemeStructure").json.pick.map(event13SchemeStructureTransformer)) and
            (__ \ "dateOfChange").json.copyFrom((uaBaseForEventType \ "changeDate").json.pick) and
            recordVersionReads("event13")
          ).reduce
      }
    }
  }


  private lazy val event14Reads = {
    (
      (__ \ "event14").readNullable[JsObject] and
        (__ \ "event14" \ "recordVersion").readNullable[JsNumber]
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

  private lazy val event18Reads = {
    (
      (__ \ "event18").readNullable[JsObject] and
        (__ \ "event18" \ "recordVersion").readNullable[JsNumber]
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

  private lazy val event19Reads: Reads[Option[JsObject]] = {
    mapReadsToOptionArray(eventTypeNodeName = "event19") { uaBaseForEventType =>
      ((__ \ "countryCode").json.copyFrom((uaBaseForEventType \ "CountryOrTerritory").json.pick) and
        (__ \ "dateOfChange").json.copyFrom((uaBaseForEventType \ "dateChangeMade").json.pick) and
        recordVersionReads("event19")
        ).reduce
    }
  }

  private lazy val event20Reads: Reads[Option[JsObject]] = {
    mapReadsToOptionArray(eventTypeNodeName = "event20") { uaBaseForEventType =>
      (uaBaseForEventType \ "whatChange").read[String].flatMap {
        case "becameOccupationalScheme" =>
          ((__ \ "occSchemeDetails" \ "startDateOfOccScheme").json
            .copyFrom((uaBaseForEventType \ "becameDate" \ "date").json.pick) and
            recordVersionReads("event20")).reduce
        case _ =>
          ((__ \ "occSchemeDetails" \ "stopDateOfOccScheme").json
            .copyFrom((uaBaseForEventType \ "ceasedDate" \ "date").json.pick) and
            recordVersionReads("event20")).reduce
      }
    }
  }


  private def schemeWindUpReads(delete: Boolean) = {
    (
      (__ \ "eventWindUp").readNullable[JsObject] and
        (__ \ "eventWindUp" \ "recordVersion").readNullable[JsNumber]
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
