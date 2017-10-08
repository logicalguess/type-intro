package logicalguess.prereq

object Implicits {

  sealed trait isAllowed[+A] //covariant means evidence for Nothing is enough

  object isAllowed {
    implicit def check[A]:isAllowed[Nothing] = null
    //implicit def checkString[A]:isAllowed[String] = null
    //implicit def checkInt[A]:isAllowed[Int] = null
  }

  def main(args: Array[String]): Unit = {
    implicitly[isAllowed[String]]
    implicitly[isAllowed[Int]]

    //implicitly[Nothing => String]

    type Non[A] = A => Nothing
    //implicitly[Non[String]]
    //implicitly[Non[Non[String]]]

    type Zero[A] = Unit => A
    implicit def zero[String]: Zero[String] = (_ => "".asInstanceOf[String])
    implicitly[Zero[String]]

    type Dual[A, B] = A => B

  }

}
