package logicalguess.prereq

object Variance {

  sealed trait Scope

  sealed trait SingleCo[+A] extends Scope
  sealed trait SingleContra[-A] extends Scope

  sealed trait PairCoCo[+A, +B] extends Scope

  trait Person
  trait Student extends Person

  trait Pet
  trait Dog extends Pet

  def main(args: Array[String]): Unit = {
    implicitly[Nothing <:< String <:< Any]
    implicitly[Nothing <:< Int <:< Any]
    implicitly[Nothing <:< Scope <:< Any]

    implicitly[SingleCo[Nothing] <:< SingleCo[String]]
    implicitly[SingleCo[String] <:< SingleCo[Any]]

    implicitly[SingleContra[Any] <:< SingleContra[String]]
    implicitly[SingleContra[String] <:< SingleContra[Nothing]]

    implicitly[PairCoCo[Nothing, Nothing] <:< PairCoCo[String, Int]]

    implicitly[Function[Any, Nothing] <:< Function[Nothing, Any]]
    implicitly[Function[Person, Dog] <:< Function[Student, Pet]]

    implicitly[Student => Person]
    implicitly[Dog => Pet]
    implicitly[Function[Person, Dog] => Function[Student, Pet]]

//    The Liskov Substitution Principle state that
//
//    If S is a subtype of T, then objects of type T may be replaced with objects of type S
//    without altering any of the desirable properties of that program.

//    e.g. assignment, overriding



  }

}
