/*
 * Copyright 2022 HM Revenue & Customs
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

object Event1Details {

  def transformToETMPData: Reads[JsObject] = ???
  /*
    (__ \ 'chargeADetails).readNullable {
      __.read(
        (((__ \ 'chargeDetails \ 'chargeTypeADetails \ 'amendedVersion).json.copyFrom((__ \ 'amendedVersion).json.pick)
          orElse doNothing) and
          (__ \ 'chargeDetails \ 'chargeTypeADetails \ 'numberOfMembers).json.copyFrom((__ \ 'chargeDetails \ 'numberOfMembers).json.pick) and
          (__ \ 'chargeDetails \ 'chargeTypeADetails \ 'totalAmtOfTaxDueAtLowerRate).json.copyFrom(
            (__ \ 'chargeDetails \ 'totalAmtOfTaxDueAtLowerRate).json.pick) and
          (__ \ 'chargeDetails \ 'chargeTypeADetails \ 'totalAmtOfTaxDueAtHigherRate).json.copyFrom(
            (__ \ 'chargeDetails \ 'totalAmtOfTaxDueAtHigherRate).json.pick) and
          (__ \ 'chargeDetails \ 'chargeTypeADetails \ 'totalAmount).json.copyFrom((__ \ 'chargeDetails \ 'totalAmount).json.pick)).reduce
      )
    }.map {
      _.getOrElse(Json.obj())
    }
   */

}

