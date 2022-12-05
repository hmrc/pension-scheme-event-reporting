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

package utils

import models.Address
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}

import java.time.LocalDate

trait ResponseGenerators extends Matchers with OptionValues {
  val ninoGen: Gen[String] = Gen.oneOf(Seq("AB123456C", "CD123456E"))

  val dateGenerator: Gen[LocalDate] = for {
    day <- Gen.choose(1, 28)
    month <- Gen.choose(1, 12)
    year <- Gen.choose(1990, 2000)
  } yield LocalDate.of(year, month, day)

  val addressGenerator: Gen[Address] = for {
    line1 <- nonEmptyString
    line2 <- nonEmptyString
    line3 <- Gen.option(nonEmptyString)
    line4 <- Gen.option(nonEmptyString)
    postalCode <- Gen.option(nonEmptyString)
    country <- Gen.listOfN(2, nonEmptyString).map(_.mkString)
  } yield {
    Address(
      line1,
      line2,
      line3,
      line4,
      postalCode,
      country
    )
  }

  def nonEmptyString: Gen[String] = Gen.alphaStr.suchThat(_.nonEmpty)

  protected def toYesNo(b: Boolean): String = if (b) "Yes" else "No"

  protected def toUserAnswersFormat(address: Address): JsObject = Json.obj(
    "addressLine1" -> address.addressLine1,
    "addressLine2" -> address.addressLine2,
    "country" -> address.country
  ) ++ address.addressLine3.fold(Json.obj()) { addr =>
    Json.obj("addressLine3" -> addr)
  } ++
    address.addressLine4.fold(Json.obj()) { addr =>
      Json.obj("addressLine3" -> addr)
    } ++
    address.postcode.fold(Json.obj()) { postcode =>
      Json.obj("postcode" -> postcode)
    }

  protected def toAPIFormat(address: Address): JsObject = Json.obj(
    "addressLine1" -> address.addressLine1,
    "addressLine2" -> address.addressLine2,
    "countryCode" -> address.country
  ) ++ address.addressLine3.fold(Json.obj()) { addr =>
    Json.obj("addressLine3" -> addr)
  } ++
    address.addressLine4.fold(Json.obj()) { addr =>
      Json.obj("addressLine3" -> addr)
    } ++
    address.postcode.fold(Json.obj()) { postcode =>
      Json.obj("postCode" -> postcode)
    }
}
