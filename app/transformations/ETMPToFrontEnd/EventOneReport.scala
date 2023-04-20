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

import play.api.libs.json._
import transformations.Transformer

//noinspection ScalaStyle
object EventOneReport extends Transformer {

  import Paths._
  import ReadsUtilities._

  
  implicit val rds1833Api: Reads[JsObject] =
    dummyUAPath.json.copyFrom(pathEtmpEventDetails.read(readsMembers))

  private lazy val readsMembers: Reads[JsArray] = __.read(Reads.seq(rdsFirstName).map(JsArray(_)))

}

private object ReadsUtilities extends Transformer {

  import Paths._

  // Dummy test reads
  implicit val rdsFirstName: Reads[JsObject] = {(
    dummyUAPath.json.copyFrom(pathEtmpIndividualMemberDetailsFirstName.json.pick)
    )}

}

private object Paths {

  /* UserAnswers */
  val dummyUAPath: JsPath = __ \ "dummy"


  /* ETMP */
  val pathEtmpEventDetails: JsPath = __ \ "event1Details"
  val pathEtmpIndividualMemberDetailsFirstName: JsPath = __ \ Symbol("individualMemberDetails")  \ Symbol("firstName")
}

