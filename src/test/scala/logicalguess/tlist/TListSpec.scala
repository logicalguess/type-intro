package logicalguess.tlist

import logicalguess.union.|
import org.scalatest._

class TListSpec extends FlatSpec with Matchers {

  class A
  case class B(name: String) extends A

  val list: TList[Nothing | Int | String | String | Int] =
    TList.empty.add(1).add("a").add("b").add(2)

  "underlying list" should "equal the list of added elements" in {
    list.values should equal(List(1, "a", "b", 2))
  }

  "contains" should "return true for added elements" in {
    list.contains("a") should be(true)
  }

  "contains" should "return false for missing elements" in {
    list.contains(7) should be(false)
  }

  "contains" should "not compile for types not in union" in {
    "list.contains(5.3)" shouldNot compile
  }

  "filter" should "return all elements of a given type" in {
    list.filter[String] should equal(List("a", "b"))
  }

  "filter" should "return an empty list for a type not added" in {
    list.filter[Double] should equal(List.empty)
  }

  "filter" should "work correctly with subtypes" in {
    val list1 = list.add[B](new B("value"))

    list1.values should equal(List(1, "a", "b", 2, B("value")))

    list1.filter[A] should equal(List(B("value")))
  }

}
