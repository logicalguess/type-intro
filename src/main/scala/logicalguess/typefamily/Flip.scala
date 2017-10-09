package logicalguess.typefamily

// operation as trait
trait Flip[T] {
  def apply(x: T): T
}

object Flip {
  // operation as function
  def flip[T](x: T)(implicit f: Flip[T]) = f(x)

  // implicits factory
  def apply[T](f: T => T): Flip[T] = x => f(x)

  // optional: implicit conversion
  implicit class Flippable[T](x: T) {
    def flip(implicit f: Flip[T]): T = f(x)
  }

  // implicits
  implicit val intFlip = Flip[Int](-_)
  implicit val doubleFlip = Flip[Double](-_)
  implicit val booleanFlip = Flip[Boolean](!_)
  implicit val stringFlip = Flip[String](_.reverse)
}


