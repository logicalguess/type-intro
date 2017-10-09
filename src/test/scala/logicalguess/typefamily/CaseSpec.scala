package logicalguess.typefamily

import org.scalatest.{FlatSpec, Matchers}

object Process extends Poly {
  implicit def intCase =
    new Case[this.type, Int] {
      type Result = Double
      def apply(num: Int): Double = num / 2.0
    }

  implicit def stringCase =
    new Case[this.type, String] {
      type Result = Int
      def apply(str: String): Int = str.length
    }
}

class CaseSpec extends FlatSpec with Matchers {

  def process[T](x: T)(implicit cse: Case[Process.type, T]): cse.Result = cse.apply(x)

  implicit class Processable[T](x: T) {
    def process(implicit cse: Case[Process.type, T]): cse.Result = cse(x)
  }

  "int args" should "be dispatched to the int case" in {
    Process(123) should equal(61.5)
    process(123) should equal(61.5)
    123.process should equal(61.5)
  }

  "string args" should "be dispatched to the string case" in {
    Process("123") should equal(3)
    process("123") should equal(3)
    "123".process should equal(3)
  }
}
