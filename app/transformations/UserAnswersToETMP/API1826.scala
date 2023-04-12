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

import play.api.libs.json._
import transformations.Transformer

object API1826 extends Transformer {

  private lazy val event10Reads = (__ \ "event10").readNullable[JsArray].map { optJsonArray =>
    optJsonArray.map { jsonArray =>
      Json.obj(
        "event10" -> jsonArray.value.map { json =>
          Json.obj(
            "recordVersion" -> (json \ "recordVersion").asOpt[String],
            "invRegScheme" -> {
              val invRegSchemePath = json \ "invRegScheme"
              Json.obj(
                "startDateDetails" -> {
                  val startDateDetailsPath = invRegSchemePath \ "startDateDetails"
                  Json.obj(
                    "startDateOfInvReg" ->  (startDateDetailsPath \ "startDateOfInvReg").asOpt[String],
                    "contractsOrPolicies" -> (startDateDetailsPath \ "contractsOrPolicies").asOpt[String]
                  )
                },
                "ceaseDateDetails" -> Json.obj(
                  "ceaseDateOfInvReg" -> (invRegSchemePath \ "ceaseDateDetails" \ "ceaseDateOfInvReg").asOpt[String]
                )
              )
            }
          )
        }
      )
    }
  }

  private lazy val event11Reads = (__ \ "event11").readNullable[JsObject].map { optJson =>
    optJson.map { json =>
      Json.obj(
        "event11" ->
          Json.obj(
            "recordVersion" -> (json \ "recordVersion").asOpt[String],
            "unauthorisedPmtsDate" -> (json \ "unauthorisedPmtsDate").asOpt[String],
            "contractsOrPoliciesDate" -> (json \ "contractsOrPoliciesDate").asOpt[String],
          )
      )
    }
  }

  private lazy val event12Reads = (__ \ "event12").readNullable[JsObject].map { optJson =>
    optJson.map { json =>
      Json.obj(
        "event12" ->
          Json.obj(
            "versionType" -> (json \ "versionType").asOpt[String],
            "twoOrMoreSchemesDate" -> (json \ "twoOrMoreSchemesDate").as[String],
          )
      )
    }
  }

  private lazy val event13Reads = (__ \ "event13").readNullable[JsArray].map { optJsonArray =>
    optJsonArray.map { jsonArray =>
      Json.obj(
        "event13" -> jsonArray.value.map { json =>
          Json.obj(
            "recordVersion" -> (json \ "recordVersion").asOpt[String],
            "schemeStructure" -> (json \ "schemeStructure").as[String],
            "schemeStructureOther" -> (json \ "schemeStructureOther").asOpt[String],
            "dateOfChange" -> (json \ "dateOfChange").as[String]
          )
        }
      )
    }
  }

  private lazy val event14Reads = (__ \ "event14").readNullable[JsObject].map { optJson =>
    optJson.map { json =>
      Json.obj(
        "event12" ->
          Json.obj(
            "versionType" -> (json \ "versionType").asOpt[String],
            "schemeMembers" -> (json \ "schemeMembers").as[String],
          )
      )
    }
  }

  private lazy val event18Reads = (__ \ "event18Confirmation").readNullable[Boolean].map {
    case Some(true) =>
      Some(
        Json.obj(
          "event18" -> Json.obj(
            "chargeablePmt" -> yes
          )
        )
      )
    case _ => None
  }

  private lazy val event19Reads = (__ \ "event19").readNullable[JsArray].map { optJsonArray =>
    optJsonArray.map { jsonArray =>
      Json.obj(
        "event19" -> jsonArray.value.map { json =>
          Json.obj(
            "recordVersion" -> (json \ "recordVersion").asOpt[String],
            "countryCode" -> (json \ "countryCode").as[String],
            "dateOfChange" -> (json \ "dateOfChange").as[String]
          )
        }
      )
    }
  }

  private lazy val event20Reads = (__ \ "event20").readNullable[JsArray].map { optJsonArray =>
    optJsonArray.map { jsonArray =>
      Json.obj(
        "event20" -> jsonArray.value.map { json =>
          Json.obj(
            "recordVersion" -> (json \ "recordVersion").asOpt[String],
            "occSchemeDetails" -> {
              val occSchemeDetailsPath = json \ "occSchemeDetails"
              Json.obj(
                "startDateOfOccScheme" -> (occSchemeDetailsPath \ "startDateOfOccScheme").asOpt[String],
                "stopDateOfOccScheme" -> (occSchemeDetailsPath \ "stopDateOfOccScheme").asOpt[String]
              )
            }
          )
        }
      )
    }
  }

  private lazy val schemeWindUpReads = (__ \ "schemeWindUpDate").readNullable[String].map {
    case Some(date) =>
      Some(
        Json.obj(
          "eventWindUp" -> Json.obj(
            "dateOfWindUp" -> date
          )
        )
      )
    case _ => None
  }



  val transformToETMPData: Reads[JsObject] = {

    def eventTypeNodes(events: Seq[JsObject]): JsObject = {
      val eventDetailNodes = events.foldLeft(Json.obj())((a, b) => a ++ b)
      if (events.isEmpty) Json.obj() else Json.obj("eventDetails" -> eventDetailNodes)
    }

    for {
      ev10 <- event10Reads
      ev11 <- event11Reads
      ev12 <- event12Reads
      ev13 <- event13Reads
      ev14 <- event14Reads
      ev18 <- event18Reads
      ev19 <- event19Reads
      ev20 <- event20Reads
      schWindUp <- schemeWindUpReads
      header <- HeaderForAllAPIs.transformToETMPData()
    } yield {
      header ++ eventTypeNodes((ev10 ++ ev11 ++ ev12 ++ ev13 ++ ev14 ++ ev18 ++ ev19 ++ ev20 ++ schWindUp).toSeq)
    }
  }
}
