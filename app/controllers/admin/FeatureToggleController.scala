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

package controllers.admin

import models.ToggleDetails
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import services.FeatureToggleService
import utils.ErrorHandler

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class FeatureToggleController @Inject()(
                                         cc: ControllerComponents,
                                         featureToggleService: FeatureToggleService
                                       )(implicit ec: ExecutionContext)
  extends AbstractController(cc) with ErrorHandler {

  def upsertFeatureToggle: Action[AnyContent] = Action.async {
    request => {
      request.body.asJson match {
        case Some(body) =>
          val toggleData = body.as[ToggleDetails]
          featureToggleService.upsertFeatureToggle(toggleData).map(_ => NoContent)
        case None =>
          Future.successful(BadRequest)
      }
    }
  }

  def deleteToggle(toggleName: String): Action[AnyContent] = Action.async {
    _ => {
      featureToggleService.deleteToggle(toggleName).map(_ => NoContent)
    }
  }

  def getToggle(toggleName: String): Action[AnyContent] = Action.async {
    _ => {
      featureToggleService.getToggle(toggleName) map {
        case Some(toggle) => Ok(Json.toJson(toggle))
        case _ => NoContent
      }
    }
  }

  def getAllFeatureToggles: Action[AnyContent] = Action.async {
    _ =>
      featureToggleService.getAllFeatureToggles map {
        seqToggles => {
          Ok(Json.toJson(seqToggles))
        }
      }
  }
}
