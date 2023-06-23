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
    "combine values correctly when both versions present with overlapping values" in {
      val a = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = false,
        Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
      )

      val b = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = true,
        Some(EROverviewVersion(numberOfVersions = 1, submittedVersionAvailable = false, compiledVersionAvailable = true))
      )
      EROverview.combine(a, b) mustBe EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = true,
        Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
      )
    }

    "combine values correctly when both versions present with mutex values" in {
      val a = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = false,
        Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
      )

      val b = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = true,
        Some(EROverviewVersion(numberOfVersions = 1, submittedVersionAvailable = true, compiledVersionAvailable = false))
      )
      EROverview.combine(a, b) mustBe EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = true,
        Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = true, compiledVersionAvailable = true))
      )
    }

    "combine values correctly when one version present (first)" in {
      val a = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = false,
        Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
      )

      val b = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = true,
        None
      )
      EROverview.combine(a, b) mustBe EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = true,
        Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
      )
    }

    "combine values correctly when both versions present (second)" in {
      val a = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = false,
        None
      )

      val b = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = true,
        Some(EROverviewVersion(numberOfVersions = 1, submittedVersionAvailable = false, compiledVersionAvailable = true))
      )
      EROverview.combine(a, b) mustBe EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = true,
        Some(EROverviewVersion(numberOfVersions = 1, submittedVersionAvailable = false, compiledVersionAvailable = true))
      )
    }

    "combine values correctly when neither versions present" in {
      val a = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = false,
        None
      )

      val b = EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = true,
        None
      )
      EROverview.combine(a, b) mustBe EROverview(
        LocalDate.of(2022, 4, 6),
        LocalDate.of(2023, 4, 5),
        tpssReportPresent = true,
        None
      )
    }

    "Seq of EROverview combine" must {
      "combine values correctly when both versions present with overlapping values where more values in first than second" in {
        val a = Seq(
          EROverview(LocalDate.of(2020, 4, 6), LocalDate.of(2021, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
          ),
          EROverview(LocalDate.of(2022, 4, 6), LocalDate.of(2023, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
          ),
          EROverview(LocalDate.of(2023, 4, 6), LocalDate.of(2024, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
          )
        )

        val b = Seq(
          EROverview(LocalDate.of(2020, 4, 6), LocalDate.of(2021, 4, 5),
            tpssReportPresent = true,
            Some(EROverviewVersion(numberOfVersions = 1, submittedVersionAvailable = true, compiledVersionAvailable = false))
          ),
          EROverview(LocalDate.of(2023, 4, 6), LocalDate.of(2024, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 5, submittedVersionAvailable = false, compiledVersionAvailable = false))
          )
        )

        EROverview.combine(a, b) mustBe Seq(
          EROverview(LocalDate.of(2020, 4, 6), LocalDate.of(2021, 4, 5),
            tpssReportPresent = true,
            Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = true, compiledVersionAvailable = true))
          ),
          EROverview(LocalDate.of(2022, 4, 6), LocalDate.of(2023, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
          ),
          EROverview(LocalDate.of(2023, 4, 6), LocalDate.of(2024, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 5, submittedVersionAvailable = false, compiledVersionAvailable = true))
          )
        )

      }

      "combine values correctly when both versions present with overlapping values where more values in second than first" in {
        val a = Seq(
          EROverview(LocalDate.of(2020, 4, 6), LocalDate.of(2021, 4, 5),
            tpssReportPresent = true,
            Some(EROverviewVersion(numberOfVersions = 1, submittedVersionAvailable = true, compiledVersionAvailable = false))
          ),
          EROverview(LocalDate.of(2023, 4, 6), LocalDate.of(2024, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 5, submittedVersionAvailable = false, compiledVersionAvailable = false))
          )
        )
        val b = Seq(
          EROverview(LocalDate.of(2020, 4, 6), LocalDate.of(2021, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
          ),
          EROverview(LocalDate.of(2022, 4, 6), LocalDate.of(2023, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
          ),
          EROverview(LocalDate.of(2023, 4, 6), LocalDate.of(2024, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
          )
        )

        EROverview.combine(a, b) mustBe Seq(
          EROverview(LocalDate.of(2020, 4, 6), LocalDate.of(2021, 4, 5),
            tpssReportPresent = true,
            Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = true, compiledVersionAvailable = true))
          ),
          EROverview(LocalDate.of(2022, 4, 6), LocalDate.of(2023, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true))
          ),
          EROverview(LocalDate.of(2023, 4, 6), LocalDate.of(2024, 4, 5),
            tpssReportPresent = false,
            Some(EROverviewVersion(numberOfVersions = 5, submittedVersionAvailable = false, compiledVersionAvailable = true))
          )
        )

      }
    }
  }

  "EROverviewVersion combine" must {
    "combine values correctly" in {
      val a = EROverviewVersion(numberOfVersions = 3, submittedVersionAvailable = true, compiledVersionAvailable = false)
      val b = EROverviewVersion(numberOfVersions = 2, submittedVersionAvailable = false, compiledVersionAvailable = true)
      EROverviewVersion.combine(a, b) mustBe EROverviewVersion(numberOfVersions = 3, submittedVersionAvailable = true, compiledVersionAvailable = true)
    }
  }
}

