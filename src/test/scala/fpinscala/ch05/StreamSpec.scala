package fpinscala.ch05

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  behavior of "headOption"

  it should "return head" in {
    val stream = Stream("a", "b", "c", "d", "e")
    val result = stream.headOption

    result should equal(Some("a"))
  }

  it should "return None when Stream is empty" in {
    val empty = Stream.empty[String]
    val result = empty.headOption

    result should equal(None)
  }

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

  behavior of "exists"

  it should "return true if condition matches." in {
    val xs = Stream("aaa", "bbb", "ccc", "ddd", "xyz")

    xs.exists(_ === "xyz") should equal(true)
  }

  behavior of "forAll"

  it should "return true if all the element of the stream satisfy the condition." in {
    val xs = Stream("aaa", "aaa", "aaa", "aaa", "aaa")
    val condition: String => Boolean = _ == "aaa"

    xs.forAll(condition) should equal(true)
  }

  it should "return false some elements does not satisfy the condition and it stops checking when the condition returns false." in {
    val xs = Stream("aaa", "aaa", "xyz", "aaa", "aaa")
    var count = 0
    val condition: String => Boolean = { x =>
      count = count + 1
      x == "aaa"
    }

    xs.forAll(condition) should equal(false)
    count should equal(3)
  }
}
