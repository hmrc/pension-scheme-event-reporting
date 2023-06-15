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

import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json._
import utils.{GeneratorAPI1828, GeneratorAPI1829, JsonFileReader}

class API1829Spec extends AnyFreeSpec with Matchers
  with JsonFileReader with GeneratorAPI1829 with GeneratorAPI1828 with ScalaCheckPropertyChecks {
  override def generateUserAnswersAndPOSTBody: Gen[(JsObject, JsObject)] = super[GeneratorAPI1829].generateUserAnswersAndPOSTBody

  "transformToETMPData" - {
    "must transform a randomly generated valid payload correctly for Event Report 20A Submit Declaration" in {
      forAll(generateUserAnswersAndPOSTBody) {
        case (userAnswers: JsObject, expectedResponse: JsObject) =>
          val result = userAnswers.validate(API1829.transformToETMPData)
          val expectedResult = JsSuccess(expectedResponse)
          result.asOpt mustBe expectedResult.asOpt
      }
    }
  }
}

