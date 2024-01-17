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

import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer

object API1829 extends Transformer {

  val transformToETMPData: Reads[JsObject] = {

    def nodes(seqOfNodes: Seq[JsObject]): JsObject = {
      seqOfNodes.foldLeft(Json.obj())((a, b) => a ++ b)
    }

    def requiredNode(node: String)(nodeName: String = node): Reads[Option[JsObject]] = {
      (__ \ node).read[String].map {
        case node: String =>
          Some(
            Json.obj(
              nodeName -> node
            )
          )
        case _ => None
      }
    }

    def optionalNode(optNode: Option[String])(nodeName: String = optNode.getOrElse("")): Reads[Option[JsObject]] = {
      (__ \ nodeName).readNullable[String].map {
        case Some(node) =>
          Some(
            Json.obj(
              nodeName -> node
            )
          )
        case _ => None
      }
    }

    def optionalDateNode(nodeName: String, startOrEndDate: String): Reads[Option[JsObject]] = {
      (__ \ nodeName).readNullable[String].map {
        case Some(node) =>
          Some(
            Json.obj(
              startOrEndDate -> node
            )
          )
        case _ => None
      }
    }

    for {
      pstrNode <- requiredNode("pstr")("pSTR")
      reportStartDateNode <- requiredNode("reportStartDate")()
      reportEndDateNode <- requiredNode("reportEndDate")()
      schemeMasterTrustStartDate <- optionalDateNode("schemeMasterTrustStartDate", "startDate")
      schemeMasterTrustCeaseDate <- optionalDateNode("schemeMasterTrustCeaseDate", "ceaseDate")
      submittedByNode <- requiredNode("submittedBy")()
      submittedIdNode <- requiredNode("submittedID")()
      psaDec1Node <- optionalNode(Some("psaDeclaration1"))()
      psaDec2Node <- optionalNode(Some("psaDeclaration2"))()
      authorisedPsaIdNode <- optionalNode(Some("authorisedPSAID"))()
      pspDec1Node <- optionalNode(Some("pspDeclaration1"))()
      pspDec2Node <- optionalNode(Some("pspDeclaration2"))()
    } yield {
      val schemeMasterTrustDetailsNode = nodes((schemeMasterTrustStartDate ++ schemeMasterTrustCeaseDate).toSeq)
      val er20ADetailsNodes = nodes((pstrNode ++ reportStartDateNode ++ reportEndDateNode).toSeq)
      val erDeclarationDetailsNodes = nodes((submittedByNode ++ submittedIdNode).toSeq)
      val psaDeclarationKeyAndNodes = ("psaDeclaration", nodes((psaDec1Node ++ psaDec2Node).toSeq))
      val pspDeclarationKeyAndNodes = ("pspDeclaration", nodes((authorisedPsaIdNode ++ pspDec1Node ++ pspDec2Node).toSeq))
      val psaOrPsp = if (authorisedPsaIdNode.nonEmpty) {
        pspDeclarationKeyAndNodes
      } else {
        psaDeclarationKeyAndNodes
      }
      Json.obj(
        "eventReportDetails" -> Json.obj(
          "er20aDetails" -> er20ADetailsNodes,
          "schemeMasterTrustDetails" -> schemeMasterTrustDetailsNode,
          "erDeclarationDetails" -> (erDeclarationDetailsNodes ++ Json.obj(psaOrPsp._1 -> psaOrPsp._2))
        )
      )
    }
  }
}
