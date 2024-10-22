package uk.gov.hmrc.pensionschemeeventreporting

import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait BaseSpec extends AnyWordSpec
  with Matchers
  with ScalaFutures
  with IntegrationPatience
  with BeforeAndAfterAll {

}
