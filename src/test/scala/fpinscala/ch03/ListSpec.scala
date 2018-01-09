package fpinscala.ch03

import List._
import org.scalatest.{ FlatSpec, Matchers }

class ListSpec extends FlatSpec with Matchers {

  // Exercise 3.1
  behavior of "pattern match for Cons"

  it should "return 3" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // match this case
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }

    x should be(3)
  }

  // Exercise 3.2
  behavior of "tail"

  it should "return list removed head in the list" in {
    val xs       = List("a", "b", "c", "d", "e")
    val expected = List("b", "c", "d", "e")
    val result   = tail(xs)

    result should equal(expected)
  }

  // Exercise 3.3
  behavior of "setHead"

  it should "replace head element of given list" in {
    val xs       = List("a", "b", "c", "d", "e")
    val input    = "x"
    val expected = List("x", "b", "c", "d", "e")

    val result = setHead(input, xs)

    result should equal(expected)
  }

  // Exercise 3.4
  behavior of "drop"

  it should "remove n elements from head of given list" in {
    val xs       = List("a", "b", "c", "d", "e")
    val n        = 3
    val expected = List("d", "e")

    val result = drop(xs, n)

    result should equal(expected)
  }
}
