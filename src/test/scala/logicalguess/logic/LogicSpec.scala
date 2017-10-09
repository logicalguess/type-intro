package logicalguess.logic

import logicalguess.logic.Logic.downcast
import logicalguess.union.|
import org.scalatest.{FlatSpec, Matchers}

object LogicSpec {

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

class LogicSpec extends FlatSpec with Matchers {

  import LogicSpec._

  "downcast" should "" in {
    val pf = downcast(pfa).orElse(downcast(pfb))

    pf(IntA(55)) should equal(55)
    pf(BoolA(true)) should equal(true)
    pf(StringB("abc")) should equal("abc")
    pf(IntB(77)) should equal(77)

    a[MatchError] should be thrownBy pf(Bool(true))
  }

  val combine2: Logic[Nothing | InputA | InputB] = Logic.union(pfa).union(pfb)

  "union" should "allow combining two partial functions" in {

    combine2(IntA(55)) should equal(55)
    combine2(BoolA(true)) should equal(true)
    combine2(StringB("abc")) should equal("abc")
    combine2(IntB(77)) should equal(77)

    "combine2(Bool(true))" shouldNot compile
  }

  val combine3: Logic[|[|[|[Nothing, InputA], InputB], Bool]] = combine2.union(pfc)

  "union" should "allow combining three partial functions" in {

    combine3(IntA(55)) should equal(55)
    combine3(BoolA(true)) should equal(true)
    combine3(StringB("abc")) should equal("abc")
    combine3(IntB(77)) should equal(77)

    combine3(Bool(true)) should equal(true)

    "logic(5)" shouldNot compile
    "logic(\"abc\")" shouldNot compile
  }


  val logicAB: Logic[InputA | InputB] = Logic.from(pfa, pfb)

  "from" should "allow building some logic from two partial functions" in {

    logicAB(IntA(55)) should equal(55)
    logicAB(BoolA(true)) should equal(true)
    logicAB(StringB("abc")) should equal("abc")
    logicAB(IntB(77)) should equal(77)

    "logicAB(Bool(true))" shouldNot compile

  }

  val logicA = Logic(pfa)
  val logicB = Logic(pfb)
  val logicC = Logic(pfc)

  val logic: Logic[|[InputA, |[InputB, Bool]]] = logicA merge (logicB merge logicC)

  "merge" should "allow combining logic" in {
    logicA(IntA(55)) should equal(55)
    "logicB(IntA(55))" shouldNot compile


    logic(IntA(55)) should equal(55)
    logic(BoolA(true)) should equal(true)
    logic(StringB("abc")) should equal("abc")
    logic(IntB(77)) should equal(77)

    logic(Bool(true)) should equal(true)

    "logic(5)" shouldNot compile
    "logic(\"abc\")" shouldNot compile
  }

  val inline: Logic[|[|[|[Nothing, InputA], InputB], Bool]] =
    Logic
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

    inline(IntA(55)) should equal(55)
    inline(BoolA(true)) should equal(true)
    inline(StringB("abc")) should equal("abc")
    inline(IntB(77)) should equal(77)

    inline(Bool(true)) should equal(true)

    "inline(5)" shouldNot compile
    "inline(\"abc\")" shouldNot compile
  }

}