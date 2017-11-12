package logicalguess.prereq

import org.scalatest.{FlatSpec, Matchers}

sealed trait isAllowed[+A] //covariant means evidence for Nothing is enough

class ImplicitsSpec extends FlatSpec with Matchers {
  "isAllowed" should "compile only for provided implicits" in {
    implicit def checkString[A]: isAllowed[String] = null

    implicitly[isAllowed[String]]

    "implicitly[isAllowed[String]]" should compile
    "implicitly[isAllowed[Int]]" shouldNot compile
  }

  "isAllowed" should "compile only for all provided implicits" in {
    implicit def checkString[A]: isAllowed[String] = null
    implicit def checkInt[A]: isAllowed[Int] = null


    implicitly[isAllowed[String]]
    implicitly[isAllowed[Int]]

    "implicitly[isAllowed[String]]" should compile
    "implicitly[isAllowed[Int]]" should compile
    "implicitly[isAllowed[Double]]" shouldNot compile
  }

  "isAllowed" should "compile for all types if evidence for Nothing is provided" in {
    implicit def check[A]: isAllowed[Nothing] = null

    implicitly[isAllowed[String]]
    implicitly[isAllowed[Int]]
    implicitly[isAllowed[Double]]

    "implicitly[isAllowed[String]]" should compile
    "implicitly[isAllowed[Int]]" should compile
    "implicitly[isAllowed[Double]]" should compile
  }

  "isAllowed" should "not compile for types with ambiguous evidence provided" in {
    implicit def ambiguous1[A]: isAllowed[Nothing] = null
    implicit def ambiguous2[A]: isAllowed[Nothing] = new isAllowed[Nothing] {}

    "implicitly[isAllowed[String]]" shouldNot compile
    "implicitly[isAllowed[Int]]" shouldNot compile
    "implicitly[isAllowed[Nothing]]" shouldNot compile
    "implicitly[isAllowed[Any]]" shouldNot compile

  }
}
