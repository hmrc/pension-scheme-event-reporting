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

package models.enumeration

sealed trait ApiType

object ApiType extends Enumerable.Implicits {

  case object Api1826 extends WithName("1826") with ApiType

  case object Api1827 extends WithName("1827") with ApiType

  case object Api1829 extends WithName("1829") with ApiType

  case object Api1830 extends WithName("1830") with ApiType

  case object Api1832 extends WithName("1832") with ApiType

  case object Api1833 extends WithName("1833") with ApiType

  case object Api1831 extends WithName("1831") with ApiType

  case object Api1834 extends WithName("1834") with ApiType

  def values: Seq[ApiType] = {
    Seq(Api1826, Api1827, Api1829, Api1830, Api1832, Api1833, Api1831, Api1834)
  }

  implicit val enumerable: Enumerable[ApiType] =
    Enumerable(values.map(v => v.toString -> v): _*)

}

