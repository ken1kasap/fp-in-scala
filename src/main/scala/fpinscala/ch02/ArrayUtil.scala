package fpinscala.ch02

import scala.annotation.tailrec

object ArrayUtil {
  // Exercise 2.2 Array[A]がソートされているかどうかを調べる
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def loop(a: A, rest: Array[A]): Boolean = {
      rest.headOption match {
        case Some(next) =>
          if (!ordered(a, next)) false
          else loop(next, rest.drop(1))
        case None => true
      }
    }

    loop(as.head, as.drop(1))
  }
}
