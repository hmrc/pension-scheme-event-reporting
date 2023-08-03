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

package models

import models.MemberChangeInfo.MemberStatus

final case class MemberChangeInfo(amendedVersion: Int, status: MemberStatus)
object MemberChangeInfo {
  trait MemberStatus {
    def name: String
  }

  final case class New() extends MemberStatus {
    def name: String = "New"
  }

  final case class Deleted() extends MemberStatus {
    def name: String = "Deleted"
  }

  final case class Changed() extends MemberStatus {
    def name: String = "Changed"
  }

  def stringToMemberStatus(memberStatus: String): MemberStatus = memberStatus match {
    case "New" => New()
    case "Deleted" => Deleted()
    case "Changed" => Changed()
    case memberStatus => throw new RuntimeException("Unknown member status: " + memberStatus)
  }
}
