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

package transformations

import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json._

trait Transformer {
  protected val doNothing: Reads[JsObject] = __.json.put(Json.obj())
  protected def fail[A]: Reads[A] = Reads.failed[A]("Unknown value")

  protected def fail[A](s: A): Reads[A] = Reads.failed[A](s"Unknown value: $s")

  protected def toYesNo(b: JsValue): JsString = if (b.as[JsBoolean].value) JsString("Yes") else JsString("No")

  protected val yes: JsString = JsString("Yes")

  protected def readsAddress(jsPath: JsPath): Reads[JsObject] =
    (
      (jsPath \ Symbol("address") \ Symbol("addressLine1")).read[String] and
        (jsPath \ Symbol("address") \ Symbol("addressLine2")).read[String] and
        (jsPath \ Symbol("address") \ Symbol("addressLine3")).readNullable[String] and
        (jsPath \ Symbol("address") \ Symbol("addressLine4")).readNullable[String] and
        (jsPath \ Symbol("address") \ Symbol("postcode")).readNullable[String] and
        (jsPath \ Symbol("address") \ Symbol("country")).read[String]
      ) (
      (addressLine1, addressLine2, addressLine3, addressLine4, postcode, country) =>
        Json.obj(
          "addressLine1" -> addressLine1,
          "addressLine2" -> addressLine2,
          "countryCode" -> country
        ) ++ addressLine3.fold(Json.obj()) { addr =>
          Json.obj("addressLine3" -> addr)
        } ++
          addressLine4.fold(Json.obj()) { addr =>
            Json.obj("addressLine3" -> addr)
          } ++
          postcode.fold(Json.obj()) { postcode =>
            Json.obj("postCode" -> postcode)
          }
    )


  protected def readsAddressEtmp(jsPathEtmp: JsPath): Reads[JsObject] =
    (
      (jsPathEtmp \  Symbol("addressLine1")).read[String] and
        (jsPathEtmp \  Symbol("addressLine2")).read[String] and
        (jsPathEtmp \  Symbol("addressLine3")).readNullable[String] and
        (jsPathEtmp \  Symbol("addressLine4")).readNullable[String] and
        (jsPathEtmp \  Symbol("postCode")).readNullable[String] and
        (jsPathEtmp \  Symbol("countryCode")).read[String]
      )(
      (addressLine1, addressLine2, addressLine3, addressLine4, postcode, country) =>
        Json.obj(
          "addressLine1" -> addressLine1,
          "addressLine2" -> addressLine2,
          "country" -> country
        ) ++ addressLine3.fold(Json.obj()) { addr =>
          Json.obj("addressLine3" -> addr)
        } ++
          addressLine4.fold(Json.obj()) { addr =>
            Json.obj("addressLine3" -> addr)
          } ++
          postcode.fold(Json.obj()) { postcode =>
            Json.obj("postcode" -> postcode)
          }
    )

}
