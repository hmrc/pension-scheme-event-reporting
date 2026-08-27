/*
 * Copyright 2026 HM Revenue & Customs
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

package models.admin

import uk.gov.hmrc.mongoFeatureToggles.model.FeatureFlagName

case object Api1826HipMigrationToggle extends FeatureFlagName {

  override val name: String = "api-1826-hip-migration-toggle"
  override val description: Option[String] = Some("Migrate API 1826 to HIP")
}

case object Api1827HipMigrationToggle extends FeatureFlagName {

  override val name: String = "api-1827-hip-migration-toggle"
  override val description: Option[String] = Some("Migrate API 1827 to HIP")
}

case object Api1828HipMigrationToggle extends FeatureFlagName {

  override val name: String = "api-1828-hip-migration-toggle"
  override val description: Option[String] = Some("Migrate API 1828 to HIP")
}

case object Api1829HipMigrationToggle extends FeatureFlagName {

  override val name: String = "api-1829-hip-migration-toggle"
  override val description: Option[String] = Some("Migrate API 1829 to HIP")
}

case object Api1830HipMigrationToggle extends FeatureFlagName {

  override val name: String = "api-1830-hip-migration-toggle"
  override val description: Option[String] = Some("Migrate API 1830 to HIP")
}

case object Api1831HipMigrationToggle extends FeatureFlagName {

  override val name: String = "api-1831-hip-migration-toggle"
  override val description: Option[String] = Some("Migrate API 1831 to HIP")
}

case object Api1832HipMigrationToggle extends FeatureFlagName {

  override val name: String = "api-1832-hip-migration-toggle"
  override val description: Option[String] = Some("Migrate API 1832 to HIP")
}

case object Api1833HipMigrationToggle extends FeatureFlagName {

  override val name: String = "api-1833-hip-migration-toggle"
  override val description: Option[String] = Some("Migrate API 1833 to HIP")
}

case object Api1834HipMigrationToggle extends FeatureFlagName {

  override val name: String = "api-1834-hip-migration-toggle"
  override val description: Option[String] = Some("Migrate API 1834 to HIP")
}
