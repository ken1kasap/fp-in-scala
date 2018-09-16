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
}
