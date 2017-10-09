package logicalguess.union

import org.scalatest.{FlatSpec, Matchers}

class UnionSpec extends FlatSpec with Matchers {
  "isPartOf" should "compile for parts of a union" in {
    implicitly[isPartOf[String, String | Int]]
    "implicitly[isPartOf[String, String | Int]]" should compile
  }


  "isPartOf" should "not compile for types that are not parts of a union" in {
    //implicitly[isPartOf[String, Double | Int]]
    "implicitly[isPartOf[String, Double | Int]]" shouldNot compile
  }
}
