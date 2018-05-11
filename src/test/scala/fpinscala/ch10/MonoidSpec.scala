package fpinscala.ch10

import fpinscala.ch10.modoids.Monoid._
import org.scalatest._

class MonoidSpec extends FlatSpec with Matchers {

  behavior of "object Monoid"

  "intAddition#op" should "return sum of the arguments." in {
    intAddition.op(1, 2) should equal(3)
  }

  "intAddition#zero" should "return 0." in {
    intAddition.zero should equal(0)
  }

  "intMultiplication#op" should "return multiplied value of the arguments." in {
    intMultiplication.op(3, 4) should equal(12)
  }

  "intMultiplication#zero" should "return 0." in {
    intMultiplication.zero should equal(0)
  }

  "booleanOr#op" should "return true even one of the arguments is given false." in {
    booleanOr.op(true, false) should equal(true)
  }

  it should "return false when both arguments are false." in {
    booleanOr.op(false, false) should equal(false)
  }

  "booleanOr#zero" should "return true." in {
    booleanOr.zero should equal(true)
  }

  "booleanAnd#op" should "return true when both arguments are true." in {
    booleanAnd.op(true, true) should equal(true)
  }

  it should "return false when one of the arguments is given false." in {
    booleanAnd.op(true, false) should equal(false)
  }

  "booleanAnd#zero" should "return false." in {
    booleanAnd.zero should equal(false)
  }
}
