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

package uk.gov.hmrc.pensionschemeeventreporting

import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.libs.ws.WSClient

class HealthEndpointIntegrationSpec extends BaseSpec with GuiceOneServerPerSuite {

  lazy val wsClient = app.injector.instanceOf[WSClient]
  lazy val baseUrl  = s"http://localhost:$port"

  override def afterAll(): Unit = {
    app.stop()
    wsClient.close()
  }

  "service health endpoint" should {
    "respond with 200 status" in {
      val response =
        wsClient
          .url(s"$baseUrl/ping/ping")
          .get()
          .futureValue

      response.status shouldBe 200
    }
  }
}
