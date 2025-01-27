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

import config.AppConfig
import play.api.libs.json.{JsValue, Json}

import javax.inject.{Inject, Singleton}

@Singleton
class DataEncryptor @Inject() (cipher: SecureGCMCipher, appConfig: AppConfig){
  def encrypt(id:String, data: JsValue):JsValue = {
    appConfig.mongoEncryptionKey.map { encryptionKey =>
      Json.toJson(cipher.encrypt(data.toString, id, encryptionKey))
    }.getOrElse(data)
  }

  def decrypt(id:String, jsValue: JsValue): JsValue = {
    jsValue.validate[EncryptedValue]
      .map { encryptedValue =>
        appConfig.mongoEncryptionKey.map { encryptionKey =>
          Json.parse(cipher.decrypt(encryptedValue, id, encryptionKey))
        }.getOrElse(throw new RuntimeException("Cannot decrypt mongoDB data. Encryption key not available."))
      }
      .getOrElse(jsValue)
  }
}
