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
import models.enumeration.EventType.{Event10, Event11}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.language.implicitConversions


object API1834 {

  implicit def reads(eventType: EventType): Reads[JsObject] = {
    eventType match {
      case Event10 => event10Reads
      case Event11 => event11Reads
      case e => Reads.failed(s"Unknown event type $e")
    }
  }

  /*
  "eventDetails" -> Json.obj(
    "event11" -> Json.obj(
      "recordVersion" -> "001",
      "unauthorisedPmtsDate" -> date1,
      "contractsOrPoliciesDate" -> date2
    )
  )
)

val expected = Json.obj(
  "event11" -> Json.obj(
    "hasSchemeChangedRulesUnAuthPayments" -> true,
    "unAuthPaymentsRuleChangeDate" -> Json.obj(
      "date" -> date1
    ),
    "hasSchemeChangedRulesInvestmentsInAssets" -> true,
    "investmentsInAssetsRuleChangeDate" -> Json.obj {
      "date" -> date2
    }
  )
)
   */

  private val event11Reads: Reads[JsObject] = {
    (__ \ "event11" \ "hasSchemeChangedRulesUnAuthPayments")
    //(__ \ "eventDetails" \ "event11")
  }


  private val event10Reads: Reads[JsObject] = {
    val event10ItemReads: Reads[JsObject] = {
      (
        (__ \ "invRegScheme" \ "startDateDetails" \ "startDateOfInvReg").readNullable[String] and
          (__ \ "invRegScheme" \ "startDateDetails" \ "contractsOrPolicies").readNullable[String] and
          (__ \ "invRegScheme" \ "ceaseDateDetails" \ "ceaseDateOfInvReg").readNullable[String]
        )(
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
