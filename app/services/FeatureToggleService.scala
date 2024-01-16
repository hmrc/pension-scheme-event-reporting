/*
 * Copyright 2024 HM Revenue & Customs
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

package services

import models._
import repositories.ToggleDataRepository

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FeatureToggleService @Inject()(
                                      toggleDataRepository: ToggleDataRepository
                                    )(implicit ec: ExecutionContext) {
  def upsertFeatureToggle(toggleDetails: ToggleDetails): Future[Unit] = {
    toggleDataRepository.upsertFeatureToggle(toggleDetails)
  }

  def deleteToggle(toggleName: String): Future[Unit] = {
    toggleDataRepository.deleteFeatureToggle(toggleName)
  }

  def getToggle(toggleName: String): Future[Option[ToggleDetails]] = {
    toggleDataRepository.getAllFeatureToggles.map {
      toggles =>
        toggles.find(_.toggleName == toggleName)
    }
  }

  def getAllFeatureToggles: Future[Seq[ToggleDetails]] = {
    toggleDataRepository.getAllFeatureToggles
  }
}