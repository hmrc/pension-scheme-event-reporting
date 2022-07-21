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

package models.enumeration

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

class EventTypeSpec extends AsyncWordSpec with Matchers {

  "apiTypeByEventTypePOST" must {

    "Return Some(Api1826) when parameter is Event10" in {
      val result = EventType.postApiTypeByEventType(eventType = EventType.Event10)
      result mustBe Some(ApiType.Api1826)
    }

    "Return Some(Api1826) when parameter is Event1" in {
      val result = EventType.postApiTypeByEventType(eventType = EventType.Event1)
      result mustBe Some(ApiType.Api1827)
    }

    "Return Some(Api1829) when parameter is Event20A" in {
      val result = EventType.postApiTypeByEventType(eventType = EventType.Event20A)
      result mustBe Some(ApiType.Api1829)
    }

    "Return Some(Api1830) when parameter is Event22" in {
      val result = EventType.postApiTypeByEventType(eventType = EventType.Event22)
      result mustBe Some(ApiType.Api1830)
    }
  }

  "apiTypeByEventTypeGET" must {

    "Return Some(Api1832) when parameter is Event3" in {
      val result = EventType.getApiTypeByEventType(eventType = EventType.Event3)
      result mustBe Some(ApiType.Api1832)
    }

    "Return Some(Api1833) when parameter is Event1" in {
      val result = EventType.getApiTypeByEventType(eventType = EventType.Event1)
      result mustBe Some(ApiType.Api1833)
    }

  }
}



