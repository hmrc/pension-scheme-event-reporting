package uk.gov.hmrc.pensionschemeeventreporting

import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneServerPerSuite
import play.api.libs.ws.WSClient

class BaseSpec extends AnyWordSpec
  with Matchers
  with ScalaFutures
  with IntegrationPatience
  with GuiceOneServerPerSuite {
  lazy val wsClient = app.injector.instanceOf[WSClient]
  lazy val baseUrl  = s"http://localhost:$port"

}
