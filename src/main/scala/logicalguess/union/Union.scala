package logicalguess.union

/**
  * http://blog.knutwalker.de/typed-actors/tut/union.html
  */

import scala.annotation.implicitNotFound

sealed trait Union

sealed trait |[+A, +B] extends Union

@implicitNotFound("Cannot prove that ${A} is a union type.")
sealed trait IsUnion[-A] {
  type Out <: Union
}

object IsUnion {
  type Aux[-A0, U0 <: Union] = IsUnion[A0] {type Out = U0}

  implicit def isUnion[A <: Union]: Aux[A, A] =
    new IsUnion[A] {
      type Out = A
    }
}

@implicitNotFound("Cannot prove that message of type ${A} is a member of ${U}.")
sealed trait isPartOf[A, +U <: Union]

object isPartOf extends IsPartOf0 {
  implicit def leftPart[A](implicit ev: A isNotA Union): isPartOf[A, A | Nothing] =
    null

  implicit def rightPart[A](implicit ev: A isNotA Union): isPartOf[A, Nothing | A] =
    null
}

sealed trait IsPartOf0 {
  implicit def tailPart1[A, U <: Union](implicit partOfTl: A isPartOf U): isPartOf[A, U | Nothing] =
    null

  implicit def tailPart2[A, U <: Union](implicit partOfTl: A isPartOf U): isPartOf[A, Nothing | U] =
    null
}

// @annotation.implicitAmbiguous("${A} must not be <: ${B}")
sealed trait isNotA[A, B]

object isNotA {
  implicit def nsub[A, B]: A isNotA B = null

  // $COVERAGE-OFF$Code only exists to prove non-equality and is expected to never execute
  implicit def nsubAmbig1[A, B >: A]: A isNotA B = sys.error("Unexpected invocation")

  implicit def nsubAmbig2[A, B >: A]: A isNotA B = sys.error("Unexpected invocation")

  // $COVERAGE-ON$
}


