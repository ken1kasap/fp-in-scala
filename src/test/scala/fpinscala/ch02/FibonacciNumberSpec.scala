package fpinscala.ch02

import org.scalatest.{ FlatSpec, Matchers }
import FibonacciNumber._

class FibonacciNumberSpec extends FlatSpec with Matchers {

  behavior of "fib"

  it should "return fib number" in {
    fib(0) should be(0)
    fib(1) should be(0)
    fib(2) should be(1)
    fib(3) should be(1)
    fib(4) should be(2)
    fib(5) should be(3)
    fib(6) should be(5)
    fib(7) should be(8)
    fib(8) should be(13)
    fib(9) should be(21)
    fib(10) should be(34)
  }
}
