package fpinscala.ch05

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  behavior of "toList"

  it should "make List from Stream" in {
    val stream = Stream("a", "b", "c", "d", "e")
    val result = stream.toList

    result should equal(List("a", "b", "c", "d", "e"))
  }
}
