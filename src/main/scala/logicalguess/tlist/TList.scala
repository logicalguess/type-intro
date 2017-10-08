package logicalguess.tlist

import logicalguess.union.{Union, isPartOf, |}
import shapeless.{TypeCase, Typeable}

import scala.reflect.runtime.universe._

case class TList[U <: Union: TypeTag](values: List[Any]) {

  def add[In : TypeTag](in: In) = TList[U | In](values :+ in)

  def contains[In](in: In)(implicit ev: isPartOf[In, U]) = values.contains(in)

  def getType = typeTag[U].tpe

  def filter[T : Typeable] = {
    val elemType = TypeCase[T]
    values.flatMap {
      case elemType(elem) => Some(elem)
        // case elem if elem.isInstanceOf[T] => Some(elem)
        // Error:(18, 37) abstract type T is unchecked since it is eliminated by erasure
      case _ => None
    }

    // values.collect { case t: T => t }
    // Error:(24, 30) abstract type pattern T is unchecked since it is eliminated by erasure

//    val pf: PartialFunction[Any, Any] = {
//      case x@(_: A | _: Int) => x
//    }
//    values.collect(pf)

//    val f: Any => Iterable[Any] = {
//      case x@(_: A | _: Int) => Some(x)
//      case _ => None
//    }
//    values flatMap f
  }
}

object TList {
  def empty = TList[Nothing](Nil)
}

class A
case class B(name: String) extends A

object TListTest {
  def main(args: Array[String]): Unit = {
    val t: TList[Nothing | Int | String | String | Int] =
      TList.empty.add(1).add("a").add("b").add(2)

    println(t.contains(7))
    // false

    println(t.getType)
    // Nothing | Int | java.lang.String | java.lang.String | Int

    println(t)
    // Typed(List(1, a, b, 2))

    println(t.filter[String])
    // List(a, b)

    println(t.filter[Double])
    // List()

    val t1 = t.add[B](new B("value"))
    println(t1)
    // Typed(List(1, a, b, 2, B(value)))

    println(t1.filter[A])
    // List(B(value))

  }
}

