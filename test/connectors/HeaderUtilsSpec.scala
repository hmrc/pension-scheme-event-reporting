/*
 * Copyright 2022 HM Revenue & Customs
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

package connectors

import config.AppConfig
import org.mockito.MockitoSugar
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HeaderUtilsSpec extends AnyWordSpec with MockitoSugar with Matchers with BeforeAndAfterEach {
  private val mockConfig = mock[AppConfig]
  private val headerUtils = new HeaderUtils(mockConfig)

  "call getCorrelationId" must {
    "return a CorrelationId of the correct size" in {
      val result = headerUtils.getCorrelationId
      result.length mustEqual headerUtils.maxLengthCorrelationIdIF
    }
  }
}
