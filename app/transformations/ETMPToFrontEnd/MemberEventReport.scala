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

import transformations.Transformer
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

object MemberEventReport extends Transformer {

  private val rdsTaxYearEndDate: Reads[JsString] = {
    (__ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("taxYearEndingDate")).json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString((str.substring(0,4).toInt - 1).toString))
      case _ => fail[JsString]
    }
  }

  implicit val rdsMemberDetailsNode: Reads[JsObject] = {
   ( (__ \ Symbol("membersDetails") \ Symbol("firstName")).json.copyFrom((__ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("individualDetails") \ Symbol("firstName")).json.pick) and
     (__ \ Symbol("membersDetails") \ Symbol("lastName")).json.copyFrom((__ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("individualDetails") \ Symbol("lastName")).json.pick) and
     (__ \ Symbol("membersDetails") \ Symbol("nino")).json.copyFrom((__ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("individualDetails") \ Symbol("nino")).json.pick) and
     (__ \ Symbol("chooseTaxYear")).json.copyFrom(rdsTaxYearEndDate) and
     (__ \ Symbol("totalPensionAmounts")).json.copyFrom((__ \ Symbol("memberDetail") \ Symbol("event") \ Symbol("paymentDetails") \ Symbol("monetaryAmount")).json.pick)).reduce
  }

  implicit val rds1832Api: Reads[JsObject] = (__ \ Symbol("event22") \ Symbol("members")).json.copyFrom((__ \ Symbol("eventDetails")).read(readsMembers))

  def readsMembers: Reads[JsArray] = __.read(Reads.seq(rdsMemberDetailsNode)).map(JsArray(_))

}

