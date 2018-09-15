package fpinscala.ch05

object Lazyness {
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def maybeTwice(b: Boolean, i: => Int): Int = {
    lazy val j = i
    if (b) j + j else 0
  }
}
