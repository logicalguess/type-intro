package logicalguess.partial

import logicalguess.union.|

object PartialExample {

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

  import Partial._

  val pf = downcast(pfa).orElse(downcast(pfb))

  def main(args: Array[String]): Unit = {
    println("function call IntA: " + pf(IntA(55)))
    println("function call BoolA: " + pf(BoolA(true)))
    println("function call StringB: " + pf(StringB("abc")))
    println("function call IntB: " + pf(IntB(77)))

    //println(pf(Bool(true))) // runtime error: MatchError

    //type Input = InputA | InputB
    //val call = Partial[Input](pf)
    val call = Partial.empty.union(pfa).union(pfb)

    println("evidence call IntA: " + call(IntA(55)))
    println("evidence call BoolA: " + call(BoolA(true)))
    println("evidence call StringB: " + call(StringB("abc")))
    println("evidence call IntB: " + call(IntB(77)))

    //call(Bool(true)) //compile error

    val chain: Partial[|[|[|[Nothing, InputA], InputB], Bool]] = Partial.union(pfa).union(pfb).union(pfc)
    println(chain(Bool(false)))
    println(chain(IntA(99)))
    println(chain(StringB("xyz")))

    val logic: Partial[|[|[|[Nothing, InputA], InputB], Bool]] = Partial
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

    println(logic(Bool(false)))
    println(logic(IntA(99)))
    println(logic(StringB("xyz")))

    // logic("abc") // compile error

  }

}