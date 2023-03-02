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

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

//import scala.language.implicitConversions


object MemberEventReport {


  implicit val rdsFor1832: Reads[JsObject] = {
   ( (__ \ Symbol("membersDetails") \ Symbol("firstName")).json.copyFrom((__ \ Symbol("individualDetails") \ Symbol("firstName")).json.pick) and
     (__ \ Symbol("membersDetails") \ Symbol("lastName")).json.copyFrom((__ \ Symbol("individualDetails") \ Symbol("lastName")).json.pick) and
     (__ \ Symbol("membersDetails") \ Symbol("nino")).json.copyFrom((__ \ Symbol("individualDetails") \ Symbol("nino")).json.pick)).reduce


    //    (__ \ Symbol("event1") \ Symbol("membersOrEmployers")).readNullable[JsArray](__.read(Reads.seq(readsMember))
    //      .map(JsArray(_))).map { optionJsArray =>
    //      val jsonArray = optionJsArray.getOrElse(Json.arr())
    //      Json.obj(
    //        "eventReportDetails" -> Json.obj(
    //          "reportStartDate" -> "2020-09-01",
    //          "reportEndDate" -> "2020-09-01"
    //        ),
    //        "event1Details" -> Json.obj(
    //          "event1Details" -> jsonArray
    //        )
    //      )
    //    }
  }


}

