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

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{__, _}

object Event1Details {
  /*
  {
     "membersOrEmployers":[
        {
           "membersDetails":{
              "firstName":"maryiah",
              "lastName":"m",
              "nino":"AB123456C"
           }
        }
     ]
  }
   */
  //def readsMember: Reads[JsObject] =
  //  ((__ \ 'memberStatus).json.copyFrom((__ \ 'memberStatus).json.pick) and
  //    (__ \ 'memberAFTVersion).json.copyFrom((__ \ 'memberAFTVersion).json.pick) and
  //    (__ \ 'memberTypeDetails).read(readsEmployerTypeDetails) and
  //    (__ \ 'correspondenceAddressDetails).read(readsCorrespondenceAddressDetails) and
  //    (__ \ 'chargeDetails \ 'paymentDate).json.copyFrom((__ \ 'dateOfPayment).json.pick) and
  //    (__ \ 'chargeDetails \ 'amountTaxDue).json.copyFrom((__ \ 'totalAmountOfTaxDue).json.pick)
  //    ).reduce

  val readsMember =
    (
      (__ \ 'individualMemberDetails \ 'firstName).json.copyFrom((__ \'membersDetails \ 'firstName).json.pick) and
        (__ \ 'individualMemberDetails \ 'lastName).json.copyFrom((__ \'membersDetails \ 'lastName).json.pick) and
        (__ \ 'individualMemberDetails \ 'nino).json.copyFrom((__ \'membersDetails \'nino).json.pick)
      ).reduce

  def readsMembers: Reads[JsArray] = __.read(Reads.seq(readsMember)).map(JsArray(_))

  def transformToETMPData: Reads[JsObject] = {
   val t = (__ \ 'membersOrEmployers).readNullable[JsArray](readsMembers).map {
      x: Option[JsArray] => x.getOrElse(Json.arr())
    }
     t.map {
      x: JsArray => Json.obj("event1Details" ->
        Json.obj("event1Details" -> x)
      )
    }
  }
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

