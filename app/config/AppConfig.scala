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

package config

import play.api.{Configuration, Environment, Mode}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.{Inject, Singleton}

@Singleton
class AppConfig @Inject()(config: Configuration, servicesConfig: ServicesConfig, runModeConfiguration: Configuration, env: Environment) {

  lazy val appName: String = config.get[String](path = "appName")

  private val ifURL: String = servicesConfig.baseUrl(serviceName = "if-hod")

  lazy val desEnvironment: String = runModeConfiguration.getOptional[String]("microservice.services.des-hod.env").getOrElse("local")
  lazy val authorization: String = "Bearer " + runModeConfiguration.getOptional[String]("microservice.services.des-hod.authorizationToken").getOrElse("local")

  lazy val integrationFrameworkEnvironment: String = runModeConfiguration.getOptional[String](
    path = "microservice.services.if-hod.env").getOrElse("local")
  lazy val integrationFrameworkAuthorization: String = "Bearer " + runModeConfiguration.getOptional[String](
    path = "microservice.services.if-hod.authorizationToken").getOrElse("local")

  val createCompileEventReportSummaryUrl: String = s"$ifURL${config.get[String](path = "serviceUrls.create-compile-event-report-summary")}"

  val compileEvent1ReportUrl: String = s"$ifURL${config.get[String](path = "serviceUrls.compile-event1-report")}"
  val compileMemberEventReportUrl: String = s"$ifURL${config.get[String](path = "serviceUrls.compile-member-event-report")}"

  def overviewUrl: String = s"$ifURL${config.get[String](path = "serviceUrls.overview")}"
  def getApiUrlByApiNum(apiNumAsString: String): String = s"$ifURL${config.get[String](path = s"serviceUrls.api$apiNumAsString")}"
  def versionUrl: String  = s"$ifURL${config.get[String](path = "serviceUrls.version")}"
  val submitEventDeclarationReportUrl: String = s"$ifURL${config.get[String](path = "serviceUrls.submit-event-declaration-report")}"
  val submitEvent20ADeclarationReportUrl: String = s"$ifURL${config.get[String](path = "serviceUrls.submit-event20a-declaration-report")}"

  val mongoEncryptionKey: Option[String] = config.getOptional[String]("mongodb.encryption.key") match {
    case None if env.mode == Mode.Prod => throw new RuntimeException("Encryption key is not set")
    case x => x
  }
}
