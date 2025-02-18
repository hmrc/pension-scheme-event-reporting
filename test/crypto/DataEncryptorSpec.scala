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

package crypto

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.crypto.EncryptedValue

class DataEncryptorSpec extends AnyFreeSpec with Matchers with GuiceOneAppPerSuite with MockitoSugar {

  private val encryptor = new DataEncryptorImpl(Some("QZNWcapID0BmWTneSk4hNl5RqdMlh4RI"))

  private val encryptorNoKey = new DataEncryptorImpl(None)

  private implicit val encryptedValueFormat: OFormat[EncryptedValue] = Json.format[EncryptedValue]

  private val id  = "id"
  private val notEncryptedJsValue = Json.parse("""{"value": true}""")
  private val encryptedJsValue  = EncryptedValue("gJBC1pxxAHYb5uUU11m2dzRqWWz50GoBO7FIPwBn",
    "W0cgl1ordmUXazSHdFzlv7McTbGwVa58xxzZYWSuqqjX+tpqm2CDvjZB3E+KhtNQTzFU8HkwcCC5bdgCaPzmG9Qai9AC7lyav9TLd/v9PNNt2f8R3rGsMHf7xxE4dndL")

  "encrypt" - {
    "must encrypt jsValue" in {
      encryptor.encrypt(id, notEncryptedJsValue).as[EncryptedValue] mustBe an[EncryptedValue]
    }
    "must not encrypt if no key is provided" in {
      encryptorNoKey.encrypt(id, notEncryptedJsValue) mustBe notEncryptedJsValue
    }
  }

  "decrypt" - {
    "must decrypt jsValue" in {
      encryptor.decrypt(id, Json.toJson(encryptedJsValue)) mustBe notEncryptedJsValue
    }

    "must return original value if not encrypted" in {
      encryptorNoKey.decrypt(id, notEncryptedJsValue) mustBe notEncryptedJsValue
    }

    "must throw a RuntimeException if no encryption field is available while decrypting a value" in {
      val exception = intercept[RuntimeException](encryptorNoKey.decrypt(id, Json.toJson(encryptedJsValue)))
      exception mustBe a[RuntimeException]
    }
  }
}
