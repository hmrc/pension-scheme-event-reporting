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

package utils

import models.Address
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.{JsObject, Json}

import java.time.LocalDate

trait ResponseGenerators extends Matchers with OptionValues {
  val ninoGen: Gen[String] = Gen.oneOf(Seq("AB123456C", "CD123456E"))

  val pstrGen: Gen[String] = for {
    numbers <- Gen.chooseNum(10000000, 99999999)
    letters <- Gen.listOfN(2, Gen.alphaChar).map(_.mkString)
  } yield s"$numbers${letters.toUpperCase}"

  val psaOrPspGen: Gen[String] = Gen.oneOf(Seq("PSA", "PSP"))

  val schemeMasterTrustStartOrCeaseDate: Gen[String] = Gen.oneOf(Seq("startDate", "ceaseDate"))

  val psaIdGen: Gen[String] = for {
    numbers <- Gen.chooseNum(10000000, 99999999)
  } yield s"A$numbers"

  val pspIdGen: Gen[String] = for {
    numbers <- Gen.chooseNum(10000000, 99999999)
  } yield s"$numbers"

  val taxYearGenerator: Gen[String] = Gen.oneOf(Seq("2022", "2023", "2024"))

  val reportStartDateGenerator: Gen[LocalDate] = for {
    year <- Gen.choose(1990, 2000)
  } yield LocalDate.of(year, 4, 6)

  val dateGenerator: Gen[LocalDate] = for {
    day <- Gen.choose(1, 28)
    month <- Gen.choose(1, 12)
    year <- Gen.choose(1990, 2000)
  } yield LocalDate.of(year, month, day)

  val dateGeneratorYMD: Gen[String] = for {
    day <- Gen.choose(1, 28)
    month <- Gen.choose(1, 12)
    year <- Gen.choose(1990, 2000)
  } yield {
    val monthFormatted = ("0" + month.toString).takeRight(2)
    val dayFormatted = ("0" + day.toString).takeRight(2)
    s"$year-$monthFormatted-$dayFormatted"
  }

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
