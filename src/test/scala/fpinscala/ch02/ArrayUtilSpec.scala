package fpinscala.ch02

import org.scalatest.{ FlatSpec, Matchers }

class ArrayUtilSpec extends FlatSpec with Matchers {

  import ArrayUtil._

  behavior of "isSorted with Int type"

  it should "return true" in {
    val as = Array(1, 2, 3, 4, 5)

    def compare(a: Int, b: Int) = a < b

    isSorted(as, compare) should be(true)
  }

  it should "return false" in {
    val as = Array(1, 2, 3, 5, 4)

    def compare(a: Int, b: Int) = a < b

    isSorted(as, compare) should be(false)
  }

  behavior of "isSorted with String type"

  it should "return true" in {
    val as = Array("a", "b", "c")

    def compare(a: String, b: String) = a.compareTo(b) < 0

    isSorted(as, compare) should be(true)

  }

  it should "return false" in {
    val as = Array("x", "z", "y")

    def compare(a: String, b: String) = a.compareTo(b) < 0

    isSorted(as, compare) should be(false)
  }
}
