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

import play.api.libs.json._

trait ReadsUtils extends Transformer {
  lazy val reqReads: (JsPath, JsPath) => Reads[JsObject] = (readToPath: JsPath, readFromPath: JsPath) =>
    readToPath.json.copyFrom(readFromPath.json.pick)

  lazy val reqNestedReadsJsString: (JsPath, Reads[JsString]) => Reads[JsObject] =
    (readToPath: JsPath, readFromPath: Reads[JsString]) => readToPath.json.copyFrom(readFromPath)

  lazy val reqNestedReadsJsBoolean: (JsPath, Reads[JsBoolean]) => Reads[JsObject] =
    (uaPath: JsPath, etmpReads: Reads[JsBoolean]) => uaPath.json.copyFrom(etmpReads)

  lazy val reqNestedReadsJsNumber: (JsPath, Reads[JsNumber]) => Reads[JsObject] =
    (uaPath: JsPath, etmpReads: Reads[JsNumber]) => uaPath.json.copyFrom(etmpReads)

  lazy val reqReadsBoolTransform: (JsPath, JsPath, String => Boolean) => Reads[JsObject] =
    (readtoPath: JsPath, readFromPath: JsPath, transform: String => Boolean) => {
      readtoPath.json.copyFrom(readFromPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsBoolean(transform(str)))
        case _ => fail[JsBoolean]
      })
    }

  lazy val reqReadsStrTransform: (JsPath, JsPath, String => String) => Reads[JsObject] =
    (uaPath: JsPath, etmpPath: JsPath, transform: String => String) => {
      uaPath.json.copyFrom(etmpPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsString(transform(str)))
        case _ => fail[JsString]
      })
    }




  lazy val optReads: (JsPath, JsPath) => Reads[JsObject] = (readToPath: JsPath, readFromPath: JsPath) =>
    readToPath.json.copyFrom(readFromPath.json.pick).orElse(doNothing)

  lazy val optNestedReadsJsString: (JsPath, Reads[JsString]) => Reads[JsObject] =
    (readToPath: JsPath, readFromPath: Reads[JsString]) => readToPath.json.copyFrom(readFromPath).orElse(doNothing)

  lazy val optNestedReadsJsArray: (JsPath, Reads[JsArray]) => Reads[JsObject] =
    (readToPath: JsPath, readFromPath: Reads[JsArray]) => readToPath.json.copyFrom(readFromPath).orElse(doNothing)

  lazy val optNestedReadsJsObject: (JsPath, Reads[JsObject]) => Reads[JsObject] =
    (etmpPath: JsPath, uaReads: Reads[JsObject]) => etmpPath.json.copyFrom(uaReads).orElse(doNothing)

  lazy val optReadsBoolTransform: (JsPath, JsPath, String => Boolean) => Reads[JsObject] =
    (readToPath: JsPath, readFromPath: JsPath, transform: String => Boolean) => {
      readToPath.json.copyFrom(readFromPath.json.pick.flatMap {
        case JsString(str) => Reads.pure(JsBoolean(transform(str)))
        case _ => fail[JsBoolean]
      }).orElse(doNothing)
    }

  lazy val optReadsDynamicPathStrTransform: (String => JsPath, JsPath, String => String) => Reads[JsObject] =
    (dynamicUaPath: String => JsPath, etmpPath: JsPath, transform: String => String) => {
      etmpPath.json.pick.flatMap {
        case JsString(str) => dynamicUaPath(transform(str)).json.copyFrom(Reads.pure(JsString(transform(str))))
        case _ => fail[JsObject]
      }.orElse(doNothing)
    }
}
