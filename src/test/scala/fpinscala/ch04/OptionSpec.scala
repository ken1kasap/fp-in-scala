package fpinscala.ch04

import org.scalatest.{ FlatSpec, Matchers }

class OptionSpec extends FlatSpec with Matchers {

  behavior of "sequence"

  it should "make Option[List[A] from List[Option[A]]" in {
    val input  = List(Some(1), Some(2), Some(3))
    val output = Some(List(1, 2, 3))

    Option.sequence(input) should equal(output)
  }

  it should "return None when None is found in the List" in {
    val input  = List(Some(1), None, Some(3))
    val output = None

    Option.sequence(input) should equal(output)
  }
}
