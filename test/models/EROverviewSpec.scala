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

package models

import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class EROverviewSpec extends AnyWordSpec with OptionValues with Matchers {

  "EROverview combine" must {
    "work" in {
      val a = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = false,
        Some(EROverviewVersion(2, submittedVersionAvailable = true, compiledVersionAvailable = true))
      )
    }
  }

  "EROverviewVersion combine" must {
    "work" in {
      val a = EROverviewVersion(2, submittedVersionAvailable = true, compiledVersionAvailable = true)
      )
    }
  }
}

