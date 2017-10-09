package logicalguess.partial

import logicalguess.partial.Partial.downcast
import logicalguess.union.|
import org.scalatest.{FlatSpec, Matchers}

object PartialSpec {

  sealed trait InputA

  case class IntA(val i: Int) extends InputA

  case class BoolA(val b: Boolean) extends InputA

  sealed trait InputB

  case class StringB(val s: String) extends InputB

  case class IntB(val i: Int) extends InputB

  case class Bool(val b: Boolean)

  val pfa: PartialFunction[InputA, Any] = {
    case IntA(i) => i
    case BoolA(b) => b
  }

  val pfb: PartialFunction[InputB, Any] = {
    case StringB(s) => s
    case IntB(i) => i
  }

  val pfc: PartialFunction[Bool, Any] = {
    case Bool(b) => b
  }
}

class PartialSpec extends FlatSpec with Matchers {

  import PartialSpec._

  "downcast" should "" in {
    val pf = downcast(pfa).orElse(downcast(pfb))

    pf(IntA(55)) should equal(55)
    pf(BoolA(true)) should equal(true)
    pf(StringB("abc")) should equal("abc")
    pf(IntB(77)) should equal(77)

    a[MatchError] should be thrownBy pf(Bool(true))
  }

  val combine2: Partial[Nothing | InputA | InputB] = Partial.union(pfa).union(pfb)

  "union" should "allow combining two partial functions" in {

    combine2(IntA(55)) should equal(55)
    combine2(BoolA(true)) should equal(true)
    combine2(StringB("abc")) should equal("abc")
    combine2(IntB(77)) should equal(77)

    "combine2(Bool(true))" shouldNot compile
  }

  val combine3: Partial[|[|[|[Nothing, InputA], InputB], Bool]] = combine2.union(pfc)

  "union" should "allow combining three partial functions" in {

    combine2(IntA(55)) should equal(55)
    combine2(BoolA(true)) should equal(true)
    combine2(StringB("abc")) should equal("abc")
    combine2(IntB(77)) should equal(77)

    combine3(Bool(true)) should equal(true)

    "logic(5)" shouldNot compile
    "logic(\"abc\")" shouldNot compile
  }


  val partialAB: Partial[InputA | InputB] = Partial.from(pfa, pfb)

  "from" should "allow building a partial from two partial functions" in {

    partialAB(IntA(55)) should equal(55)
    partialAB(BoolA(true)) should equal(true)
    partialAB(StringB("abc")) should equal("abc")
    partialAB(IntB(77)) should equal(77)

    "partialAB(Bool(true))" shouldNot compile

  }

  val partialA = Partial.from(pfa)
  val partialB = Partial.from(pfb)
  val partialC = Partial.from(pfc)

  val partial = partialA merge (partialB merge partialC) // right associative

  "merge" should "allow combining partials" in {

    partial(IntA(55)) should equal(55)
    partial(BoolA(true)) should equal(true)
    partial(StringB("abc")) should equal("abc")
    partial(IntB(77)) should equal(77)

    partial(Bool(true)) should equal(true)

    "logic(5)" shouldNot compile
    "logic(\"abc\")" shouldNot compile
  }

  val logic: Partial[|[|[|[Nothing, InputA], InputB], Bool]] =
    Partial
      .union[InputA] {
      case IntA(i) => i
      case BoolA(b) => b
    }
      .union[InputB] {
      case StringB(s) => s
      case IntB(i) => i
    }
      .union[Bool] {
      case Bool(b) => b
    }

  "union" should "allow chaining partial functions" in {

    logic(IntA(55)) should equal(55)
    logic(BoolA(true)) should equal(true)
    logic(StringB("abc")) should equal("abc")
    logic(IntB(77)) should equal(77)

    logic(Bool(true)) should equal(true)

    "logic(5)" shouldNot compile
    "logic(\"abc\")" shouldNot compile
  }

}