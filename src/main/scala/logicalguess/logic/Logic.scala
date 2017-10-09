package logicalguess.logic

import logicalguess.union.{Union, isPartOf, |}

import scala.reflect.ClassTag

class LogicUnit[-U](val pf: PartialFunction[Any, Any]) {
  def union[In](f: PartialFunction[In, Any])(implicit ct: ClassTag[In]): Logic[U | In] =
    Logic[U | In](pf.orElse(Logic.downcast(f)))

  def merge[V](p: LogicUnit[V]): Logic[U | V] = union(p.pf)

  def apply[In](in: In): Any = pf(in)
}

case class Logic[-U <: Union](override val pf: PartialFunction[Any, Any]) extends LogicUnit[U](pf) {
  def apply[In](in: In)(implicit ev: isPartOf[In, U]): Any = pf(in)
}

object Logic {
  def empty =
    Logic[Nothing | Nothing](emptyFunction)


  def apply[I: ClassTag, O](pf: PartialFunction[I, O]) = {
    new LogicUnit[I](downcast(pf))
  }

  def from[I: ClassTag, O](pf: PartialFunction[I, O]) = {
    new Logic[Nothing | I](downcast(pf))
  }

  def from[I1: ClassTag, I2: ClassTag, O1, O2](pf1: PartialFunction[I1, O1], pf2: PartialFunction[I2, O2]) = {
    new Logic[I1 | I2](downcast(pf1).orElse(downcast(pf2)))
  }

  def union[A](f: Function[A, Any])(implicit ct: ClassTag[A]) =
    Logic[Nothing | A](emptyFunction.orElse(downcast(f)))

  def downcast[In](f: In => Any)(implicit ct: ClassTag[In]): PartialFunction[Any, Any] = {
    case (in: In) => f(in)
  }
}

object emptyFunction extends PartialFunction[Any, Any] {
  def isDefinedAt(x: Any) = false

  def apply(x: Any) = throw new UnsupportedOperationException("Empty function")
}

