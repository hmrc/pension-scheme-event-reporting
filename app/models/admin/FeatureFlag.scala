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

import models.enumeration.ApiType
import models.enumeration.ApiType.*
import uk.gov.hmrc.mongoFeatureToggles.model.FeatureFlagName

trait HipMigrationToggle {
  def name(api: ApiType): String = s"api-${api.toString}-hip-migration-toggle"
  
  def desc(api: ApiType): Option[String] = Some(s"Migrate API ${api.toString} to HIP")
}

case object Api1826HipMigrationToggle extends FeatureFlagName with HipMigrationToggle {

  override val name: String = name(Api1826)
  override val description: Option[String] = desc(Api1826)
}

case object Api1827HipMigrationToggle extends FeatureFlagName with HipMigrationToggle {

  override val name: String = name(Api1827)
  override val description: Option[String] = desc(Api1827)
}

case object Api1828HipMigrationToggle extends FeatureFlagName with HipMigrationToggle {

  override val name: String = name(Api1828)
  override val description: Option[String] = desc(Api1828)
}

case object Api1829HipMigrationToggle extends FeatureFlagName with HipMigrationToggle {

  override val name: String = name(Api1829)
  override val description: Option[String] = desc(Api1829)
}

case object Api1830HipMigrationToggle extends FeatureFlagName with HipMigrationToggle {

  override val name: String = name(Api1830)
  override val description: Option[String] = desc(Api1830)
}

case object Api1831HipMigrationToggle extends FeatureFlagName with HipMigrationToggle {

  override val name: String = name(Api1831)
  override val description: Option[String] = desc(Api1831)
}

case object Api1832HipMigrationToggle extends FeatureFlagName with HipMigrationToggle {

  override val name: String = name(Api1832)
  override val description: Option[String] = desc(Api1832)
}

case object Api1833HipMigrationToggle extends FeatureFlagName with HipMigrationToggle {

  override val name: String = name(Api1833)
  override val description: Option[String] = desc(Api1833)
}

case object Api1834HipMigrationToggle extends FeatureFlagName with HipMigrationToggle {

  override val name: String = name(Api1834)
  override val description: Option[String] = desc(Api1834)
}
