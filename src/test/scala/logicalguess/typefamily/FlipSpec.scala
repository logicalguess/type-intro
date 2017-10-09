package logicalguess.typefamily

import org.scalatest.{FlatSpec, Matchers}


class FlipSpec extends FlatSpec with Matchers {
  import Flip._

  "int args" should "be dispatched to the int implicit" in {
    flip(123) should equal(-123)
    123.flip should equal(-123)
  }

  "boolean args" should "be dispatched to the boolean implicit" in {
    flip(true) should equal(false)
    true.flip should equal(false)
  }

  "string args" should "be dispatched to the string implicit" in {
    flip("abc") should equal("cba")
    "abc".flip should equal("cba")
  }
}
