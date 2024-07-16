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

package transformations

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.libs.json._

//noinspection ScalaStyle
class ReadsUtilsSpec extends AnyWordSpec with ReadsUtils with MockitoSugar with Matchers {

  private val jsonToRead = Json.obj("test" -> "test")
  private val readToPath: JsPath = __ \ "toPath"
  private val readFromPath: JsPath = __ \ "fromPath"

  private val readsJsString: Reads[JsString] = {
    readFromPath.json.pick.flatMap {
      case JsString(str) => Reads.pure(JsString(str))
      case _ => fail(JsString("reads JsString failed"))
    }
  }

  private val readsJsBoolean: Reads[JsBoolean] = {
    readFromPath.json.pick.flatMap {
      case JsString("Yes") => Reads.pure(JsBoolean(true))
      case _ => Reads.pure(JsBoolean(false))
    }
  }

  private val readsJsNumber: Reads[JsNumber] = {
    readFromPath.json.pick.flatMap {
      case JsString(num) => Reads.pure(JsNumber(num.toInt))
      case _ => Reads.pure(JsNumber(0))
    }
  }

  "reqReads" must {
    "throw an error if a value is missing" in {
      val reads = reqReads(readToPath, readFromPath)
      val result = jsonToRead.validate(reads)

      result mustBe a [JsError]
    }
  }

  "reqNestedReadsJsString" must {
    "throw an error if a value is missing" in {
      val reads = reqNestedReadsJsString(readToPath, readsJsString)
      val result = jsonToRead.validate(reads)

      result mustBe a [JsError]
    }
  }

  "reqNestedReadsJsBoolean" must {
    "throw an error if a value is missing" in {
      val reads = reqNestedReadsJsBoolean(readToPath, readsJsBoolean)
      val result = jsonToRead.validate(reads)

      result mustBe a [JsError]
    }
  }

  "reqNestedReadsJsNumber" must {
    "throw an error if a value is missing" in {
      val reads = reqNestedReadsJsNumber(readToPath, readsJsNumber)
      val result = jsonToRead.validate(reads)

      result mustBe a [JsError]
    }
  }

  "reqReadsBoolTransform" must {
    "throw an error if a value is missing" in {
      val reads = reqReadsBoolTransform(readToPath, readFromPath, yesNoTransformToBoolean)
      val result = jsonToRead.validate(reads)

      result mustBe a [JsError]
    }
  }

  "reqReadsStrTransform" must {
    "throw an error if a value is missing" in {
      lazy val stringTransform: String => String = {
        case "Cat" => "meow"
        case "Dog" => "woof"
      }

      val reads = reqReadsStrTransform(readToPath, readFromPath, stringTransform)
      val result = jsonToRead.validate(reads)

      result mustBe a [JsError]
    }
  }
}
