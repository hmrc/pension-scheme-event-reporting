package uk.gov.hmrc.pensionschemeeventreporting

import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.libs.ws.WSClient

class UserLockSpec extends AnyWordSpec
  with Matchers
  with ScalaFutures
  with IntegrationPatience
  with GuiceOneServerPerSuite
  with BeforeAndAfterAll {
  val wsClient = app.injector.instanceOf[WSClient]
  val baseUrl  = s"http://localhost:$port"

  override def beforeAll(): Unit = {

  }
  override def afterAll(): Unit = {
    wsClient.close()
  }
}
