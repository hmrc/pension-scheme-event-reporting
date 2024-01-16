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

package services

import play.api.libs.json.JsObject
import models.MemberChangeInfo
import models.MemberChangeInfo._

import javax.inject.Singleton

@Singleton
class MemberChangeInfoService() {

  //scalastyle:off cyclomatic.complexity
  def generateMemberChangeInfo(oldMember: Option[JsObject],
                                       newMember: JsObject,
                                       currentVersion: Int): Option[MemberChangeInfo] = {

    def noVersion(obj: JsObject) = obj - "amendedVersion" - "memberStatus"

    def version(obj: JsObject) = obj.value.get("amendedVersion").map(_.as[String].toInt)

    def status(obj: JsObject) = obj.value.get("memberStatus").map(_.as[String]).map(stringToMemberStatus)

    val newMemberStatus = status(newMember).getOrElse(New())

    oldMember.map { oldMember =>
      val oldMemberNoVersion = noVersion(oldMember)
      val newMemberNoVersion = noVersion(newMember)
      val memberChanged = oldMemberNoVersion != newMemberNoVersion
      val oldMemberVersion = version(oldMember).getOrElse(currentVersion)
      val hasSameVersion = version(oldMember).contains(currentVersion)
      val oldMemberStatus = status(oldMember).getOrElse(New())
      val memberChangeInfo = (hasSameVersion, oldMemberStatus, newMemberStatus) match {
        case (_, Deleted(), Deleted()) => Some(MemberChangeInfo(oldMemberVersion, Deleted()))
        case (true, New(), Deleted()) => None
        case (_, _, Deleted()) => Some(MemberChangeInfo(currentVersion, Deleted()))
        case _ =>
          val memberChangeInfo = (hasSameVersion, memberChanged, oldMemberStatus) match {
            case (false, false, oldMemberStatus) => MemberChangeInfo(oldMemberVersion, oldMemberStatus)
            case (true, _, New()) => MemberChangeInfo(oldMemberVersion, New())
            case (false, true, _) => MemberChangeInfo(currentVersion, Changed())
            case (true, false, oldMemberStatus) => MemberChangeInfo(oldMemberVersion, oldMemberStatus)
            case (true, true, oldMemberStatus) => MemberChangeInfo(oldMemberVersion, oldMemberStatus)
          }
          Some(memberChangeInfo)
      }
      memberChangeInfo
    }.getOrElse({
      if (newMemberStatus == Deleted()) None else Some(MemberChangeInfo(currentVersion, newMemberStatus))
    })
  }
}
