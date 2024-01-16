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

import models.MemberChangeInfo
import models.MemberChangeInfo._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.Application
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsObject, Json}

class MemberChangeInfoServiceSpec extends AsyncWordSpec with Matchers with MockitoSugar {
  val application: Application = new GuiceApplicationBuilder()
    .configure(conf = "auditing.enabled" -> false, "metrics.enabled" -> false, "metrics.jvm" -> false).build()
  private def service = application.injector.instanceOf[MemberChangeInfoService]

  private def generateJson(memberStatus: Option[models.MemberChangeInfo.MemberStatus], amendedVersion: Int) =
    Json.parse(s"""
      |{
      | "amendedVersion": "${("00" + amendedVersion.toString).takeRight(3)}"
      | ${memberStatus.map{x => s""", "memberStatus": "${x.name}" """}.getOrElse("") }
      |}
      |""".stripMargin).as[JsObject]

  private def generateJsonNew(memberStatus: Option[models.MemberChangeInfo.MemberStatus], amendedVersion: Int, different: Boolean = false) =
    Json.parse(
      s"""
         |{
         | "amendedVersion": "${("00" + amendedVersion.toString).takeRight(3)}"
         | ${memberStatus.map{x => s""", "memberStatus": "${x.name}" """}.getOrElse("") }
         | ${if (different) """, "changed": true """ else ""}
         |}
         |""".stripMargin).as[JsObject]

  "MemberChangeInfoService" must {
    "return old amendedVersion when deleted was sent with new version" in {
      val o = generateJson(Some(Deleted()), 1)
      val n = generateJsonNew(Some(Deleted()), 2)
      val expected = Some(MemberChangeInfo(1, Deleted()))
      service.generateMemberChangeInfo(Some(o), n, 2) mustBe expected
    }

    "return nothing when new member deleted during compile" in {
      val o = generateJson(Some(New()), 1)
      val n = generateJsonNew(Some(Deleted()), 1)
      val expected = None
      service.generateMemberChangeInfo(Some(o), n, 1) mustBe expected
    }

    "return deleted when past compile is not deleted" in {
      val o = generateJson(Some(New()), 1)
      val n = generateJsonNew(Some(Deleted()), 2)
      val expected = Some(MemberChangeInfo(2, Deleted()))
      service.generateMemberChangeInfo(Some(o), n, 2) mustBe expected
    }

    "return same status and version when no change have been made" in {
      val o = generateJson(Some(Changed()), 1)
      val n = generateJsonNew(None, 2)
      val expected = Some(MemberChangeInfo(1, Changed()))
      service.generateMemberChangeInfo(Some(o), n, 2) mustBe expected
    }

    "return keep new status when changed during the same compile" in {
      val o = generateJson(Some(New()), 1)
      val n = generateJsonNew(None, 1, different = true)
      val expected = Some(MemberChangeInfo(1, New()))
      service.generateMemberChangeInfo(Some(o), n, 1) mustBe expected
    }

    "return change status if member changed in the new version" in {
      val o = generateJson(Some(New()), 1)
      val n = generateJsonNew(None, 2, different = true)
      val expected = Some(MemberChangeInfo(2, Changed()))
      service.generateMemberChangeInfo(Some(o), n, 2) mustBe expected
    }

    "return same status during same compile if status has not been changed" in {
      val o = generateJson(Some(Changed()), 1)
      val n = generateJsonNew(None, 1)
      val expected = Some(MemberChangeInfo(1, Changed()))
      service.generateMemberChangeInfo(Some(o), n, 1) mustBe expected
    }

    "return same status and version if nothing changed" in {
      val o = generateJson(Some(Changed()), 1)
      val n = generateJsonNew(None, 1)
      val expected = Some(MemberChangeInfo(1, Changed()))
      service.generateMemberChangeInfo(Some(o), n, 1) mustBe expected
    }

    "return same status and version if changed during compile" in {
      val o = generateJson(Some(Changed()), 1)
      val n = generateJsonNew(None, 1, different = true)
      val expected = Some(MemberChangeInfo(1, Changed()))
      service.generateMemberChangeInfo(Some(o), n, 1) mustBe expected
    }

    "return new if old member did not include version" in {
      val o = generateJson(None, 1)
      val n = generateJsonNew(None, 1, different = true)
      val expected = Some(MemberChangeInfo(1, New()))
      service.generateMemberChangeInfo(Some(o), n, 1) mustBe expected
    }

    "return nothing if old member not available and new member is deleted" in {
      val n = generateJsonNew(Some(Deleted()), 1)
      val expected = None
      service.generateMemberChangeInfo(None, n, 1) mustBe expected
    }

    "return same new status if old member not available" in {
      val n = generateJsonNew(None, 1)
      val expected = Some(MemberChangeInfo(1, New()))
      service.generateMemberChangeInfo(None, n, 1) mustBe expected
    }

    "return new status if original is empty" in {
      val n = generateJsonNew(None, 1)
      val expected = Some(MemberChangeInfo(1, New()))
      service.generateMemberChangeInfo(Some(Json.parse("{}").as[JsObject]), n, 1) mustBe expected
    }
  }
}
