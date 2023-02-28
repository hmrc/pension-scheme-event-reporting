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

import play.api.libs.json.Reads._
import play.api.libs.json._
import transformations.Transformer

object API1828 extends Transformer {

  val transformToETMPData: Reads[JsObject] = {

    def nodes(seqOfNodes: Seq[JsObject]): JsObject = {
      seqOfNodes.foldLeft(Json.obj())((a, b) => a ++ b)
    }

    def requiredNode(node: String): Reads[Option[JsObject]] = {
      val nameOfNode = if (node == "pstr") { "pSTR" } else { node }
      (__ \ node).readNullable[String].map {
      case Some(node) =>
        Some(
          Json.obj(
            nameOfNode -> node
          )
        )
      case _ => None
    }
    }

    def optionalNode(optNode: String) = {
      val nameOfOptNode = optNode
      (__ \ optNode).readNullable[String].map {
        case Some(optNode) =>
          Some(
            Json.obj(
              nameOfOptNode -> optNode
            )
          )
        case _ => None
      }
    }

    for {
      pstrNode <- requiredNode("pstr")
      reportStartDateNode <- requiredNode("reportStartDate")
      reportEndDateNode <- requiredNode("reportEndDate")
      submittedByNode <- requiredNode("submittedBy")
      submittedIdNode <- requiredNode("submittedID")
      psaDec1Node <- optionalNode("psaDeclaration1")
      psaDec2Node <- optionalNode("psaDeclaration2")
      authorisedPsaIdNode <- optionalNode("authorisedPSAID")
      pspDec1Node <- optionalNode("pspDeclaration1")
      pspDec2Node <- optionalNode("pspDeclaration2")
    } yield {
      val erDetailsNodes = nodes((pstrNode ++ reportStartDateNode ++ reportEndDateNode).toSeq)
      val erDeclarationDetailsNodes = nodes((submittedByNode ++ submittedIdNode).toSeq)
      val psaDeclarationKeyAndNodes = ("psaDeclaration", nodes((psaDec1Node ++ psaDec2Node).toSeq))
      val pspDeclarationKeyAndNodes = ("pspDeclaration", nodes((authorisedPsaIdNode ++ pspDec1Node ++ pspDec2Node).toSeq))
      val psaOrPsp = {
        if (authorisedPsaIdNode.headOption == None) {
          psaDeclarationKeyAndNodes
        }
        else {
          pspDeclarationKeyAndNodes
        }
      }
      Json.obj(
        "declarationDetails" -> Json.obj(
          "erDetails" -> erDetailsNodes,
          "erDeclarationDetails" -> erDeclarationDetailsNodes,
          psaOrPsp._1 -> psaOrPsp._2
        )
      )
    }
  }
}

//  private val test: Reads[JsObject] = (__ \ "psaDeclaration").readNullable[String].map {
//    case Some(_) =>
//      // PSA only:
//      (__ \ "declarationDetails" \ "psaDeclaration" \ "psaDeclaration1").json.copyFrom((__ \ "psaDeclaration1").json.pick) and
//        (__ \ "declarationDetails" \ "psaDeclaration" \ "psaDeclaration2").json.copyFrom((__ \ "psaDeclaration2").json.pick)
//    case _ =>
//      // PSP only:
//    (__ \ "declarationDetails" \ "pspDeclaration" \ "authorisedPSAID").json.copyFrom((__ \ "authorisedPSAID").json.pick) and
//    (__ \ "declarationDetails" \ "pspDeclaration" \ "pspDeclaration1").json.copyFrom((__ \ "pspDeclaration1").json.pick) and
//    (__ \ "declarationDetails" \ "pspDeclaration" \ "pspDeclaration2").json.copyFrom((__ \ "pspDeclaration2").json.pick)
//  }

//  val transformToETMPData: Reads[JsObject] = {
//    (
//      // These are all common between PSAs and PSPs.
//      (__ \ "declarationDetails" \ "erDetails" \ "pSTR").json.copyFrom((__ \ "pstr").json.pick) and
//        (__ \ "declarationDetails" \ "erDetails" \ "reportStartDate").json.copyFrom((__ \ "reportStartDate").json.pick) and
//        (__ \ "declarationDetails" \ "erDetails" \ "reportEndDate").json.copyFrom((__ \ "reportEndDate").json.pick) and
//        (__ \ "declarationDetails" \ "erDeclarationDetails" \ "submittedBy").json.copyFrom((__ \ "submittedBy").json.pick) and
//        (__ \ "declarationDetails" \ "erDeclarationDetails" \ "submittedID").json.copyFrom((__ \ "submittedID").json.pick)
//      ).reduce
//  }

