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
  }

}
