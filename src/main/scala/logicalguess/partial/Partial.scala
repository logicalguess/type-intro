package logicalguess.partial

import logicalguess.union.{Union, isPartOf, |}

import scala.reflect.ClassTag


case class Partial[U <: Union](pf: PartialFunction[Any, Any]) {

  import Partial._

  def union[In](f: PartialFunction[In, Any])(implicit ct: ClassTag[In]) =
    Partial[U | In](pf.orElse(downcast(f)))

  def apply[In](in: In)(implicit ev: isPartOf[In, U]): Any = pf(in)
}

object Partial {
  def empty =
    Partial[Nothing | Nothing](emptyFunction)

  def union[A](f: Function[A, Any])(implicit ct: ClassTag[A]) =
    Partial[Nothing | A](emptyFunction.orElse(downcast(f)))

  def downcast[In](f: In => Any)(implicit ct: ClassTag[In]): PartialFunction[Any, Any] = {
    case (in: In) => f(in)
  }
}

object emptyFunction extends PartialFunction[Any, Any] {
  def isDefinedAt(x: Any) = false

  def apply(x: Any) = throw new UnsupportedOperationException("Empty function")
}

