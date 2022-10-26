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

package repositories

import de.flapdoodle.embed.mongo.config.{MongodConfig, Net}
import de.flapdoodle.embed.mongo.distribution.Version
import de.flapdoodle.embed.mongo.{MongodExecutable, MongodProcess, MongodStarter}
import de.flapdoodle.embed.process.runtime.Network

import scala.util.Random

trait EmbeddedMongoDBSupport {

  val mongoHost = "localhost"
  val mongoPort = 10000 + Random.nextInt(10000)

  var mongodExecutable: MongodExecutable = _

  def startMongoD(): MongodProcess =
    mongodExecutable.start()

  def stopMongoD(): Unit =
    mongodExecutable.stop()

  def initMongoDExecutable(): Unit =
    mongodExecutable = MongodStarter.getDefaultInstance
      .prepare(MongodConfig.builder()
        .version(Version.Main.V4_2)
        .net(new Net(mongoHost, mongoPort, Network.localhostIsIPv6()))
        .build()
      )
}