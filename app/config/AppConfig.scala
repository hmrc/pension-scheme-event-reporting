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

import models.enumeration.ApiType
import play.api.Configuration
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.Duration

@Singleton
class AppConfig @Inject()(config: Configuration, servicesConfig: ServicesConfig) {

  lazy val appName: String =
    config.get[String]("appName")

  private val ifURL: String =
    servicesConfig.baseUrl("if-hod")
  private val hipURL: String =
    s"${servicesConfig.baseUrl("hip-hod")}/RESTAdapter"
    
  val ifsTimeout: Duration =
    config.get[Duration]("ifs.timeout")
  
  lazy val integrationFrameworkEnvironment: String =
    config.getOptional[String]("microservice.services.if-hod.env").getOrElse("local")
  lazy val integrationFrameworkAuthorization: String =
    s"Bearer ${config.getOptional[String]("microservice.services.if-hod.authorizationToken").getOrElse("local")}"

  def apiUrl(apiType: ApiType, hip: Boolean = false): String =
    s"${if (hip) hipURL else ifURL}${config.get[String](s"serviceUrls.api${apiType.toString}")}"
  def overviewUrl: String =
    s"$ifURL${config.get[String]("serviceUrls.overview")}"
  def versionUrl: String  =
    s"$ifURL${config.get[String]( "serviceUrls.version")}"
  lazy val minimalPsaDetailsUrl: String =
    s"$pensionsAdministratorUrl${config.get[String]("serviceUrls.minimalPsaDetails")}"
  private val baseUrlPensionsScheme: String =
    servicesConfig.baseUrl("pensions-scheme")
  val checkAssociationUrl: String =
    s"$baseUrlPensionsScheme${config.underlying.getString("serviceUrls.checkPsaAssociation")}"
  val pensionsAdministratorUrl: String =
    servicesConfig.baseUrl("pension-administrator")
  val mongoEncryptionKey: Option[String] =
    config.getOptional[String]("mongodb.encryption.key")
}
