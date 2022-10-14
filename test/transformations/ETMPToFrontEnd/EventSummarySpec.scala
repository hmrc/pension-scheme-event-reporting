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

package transformations.ETMPToFrontEnd

import org.mockito.MockitoSugar
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import uk.gov.hmrc.http._
import utils.JsonFileReader

class EventSummarySpec extends AsyncWordSpec with Matchers with MockitoSugar with BeforeAndAfter with JsonFileReader {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  before {


  }


//TODO: PODS-7683 below test will fail until transformation code is written

//  "Reads" must {
//    "transform a valid payload correctly" in {
//      val json = readJsonFromFile("/api-1834-valid-example.json")
//      val result = json.validate(EventSummary.rds).asOpt
//
//      val resultAsSet = result.toSet
//
//      resultAsSet mustBe Some(
//        Set(
//          Event10,
//          Event11,
//          Event12,
//          Event13,
//          Event14,
//          Event18,
//          Event19,
//          Event20,
//          WindUp,
//          Event2,
//          Event4,
//          Event24
//        )
//      )
//    }
//  }
}
