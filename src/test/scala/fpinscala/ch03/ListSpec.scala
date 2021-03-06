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

  // Exercise 3.5
  behavior of "dropWhile"

  it should "return new list that matched elements were dropped." in {
    val xs       = List("a", "a", "a", "d", "e")
    val expected = List("d", "e")
    val result   = dropWhile(xs, (x: String) => x == "a")

    result should equal(expected)
  }

  it should "return the input list if none of character matches." in {
    val xs     = List("a", "b", "c", "d", "e")
    val result = dropWhile(xs, (x: String) => x == "x")

    result should equal(xs)
  }

  // Exercise 3.6
  behavior of "init"

  it should "return the list that was deleted last element." in {
    val xs       = List(1, 2, 3, 4, 5)
    val expected = List(1, 2, 3, 4)
    val result   = init(xs)

    result should equal(expected)
  }

  // Exercise 3.9
  behavior of "length"

  it should "return the length of the list." in {
    val xs       = List(1, 2, 3, 4, 5)
    val expected = 5
    val result   = List.length(xs)

    result should equal(expected)
  }

  it should "return 0 when the list is empty." in {
    val xs       = List()
    val expected = 0
    val result   = List.length(xs)

    result should equal(expected)
  }

  // Exercise 3.10
  behavior of "foldLeft"

  it should "concat String from the head of the list." in {
    val xs       = List("f", "o", "l", "d", "L", "e", "f", "t")
    val expected = "foldLeft"
    val result   = foldLeft(xs, "")(_ + _)

    result should equal(expected)
  }

  // Exercise 3.11
  behavior of "sum3"

  it should "return total value of the list." in {
    val xs       = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val expected = 55

    val result = sum3(xs)

    result should equal(expected)
  }

  behavior of "product3"

  it should "return multiplied value of the list." in {
    val xs       = List(1.0, 2.0, 3.0, 4.0, 5.0)
    val expected = 120.0

    val result = product3(xs)

    result should equal(expected)
  }

  behavior of "length2"

  it should "return the length of the list." in {
    val xs       = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    val expected = 11

    val result = length2(xs)

    result should equal(expected)
  }
}
