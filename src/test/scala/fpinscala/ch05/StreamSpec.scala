package fpinscala.ch05

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  behavior of "toList"

  it should "make List from Stream" in {
    val stream = Stream("a", "b", "c", "d", "e")
    val result = stream.toList

    result should equal(List("a", "b", "c", "d", "e"))
  }

  behavior of "take"

  it should "return 3 elements of Stream when parameter 3 is passed." in {
    val xs = Stream("a", "b", "c", "d", "e")
    val expected = List("a", "b", "c")

    xs.take(3).toList should equal(expected)
  }

  behavior of "drop"

  it should "remove n elements from head of given stream" in {
    val xs = Stream("a", "b", "c", "d", "e")
    val expected = List("d", "e")

    xs.drop(3).toList should equal(expected)
  }

  behavior of "takeWhile"

  it should "return new Stream which is made until elements match condition from head." in {
    val xs = Stream("aaa", "aaa", "bbb", "ccc", "aaa")
    val condition: String => Boolean = _ == "aaa"

    val expected = List("aaa", "aaa")

    xs.takeWhile(condition).toList should equal(expected)
  }

  it should "return empty Stream when head did not match the condition." in {
    val xs = Stream("abc", "aaa", "bbb", "ccc", "aaa")
    val condition: String => Boolean = _ == "aaa"

    xs.takeWhile(condition).toList should equal(Nil)
  }
}
