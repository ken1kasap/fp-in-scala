package fpinscala.ch05

import scala.annotation.tailrec

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  // Exercise 5.1
  // Need improvement. Should implement tail recursive.
  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = {
    def loop(a: Int, xs: Stream[A]): Stream[A] =
      if (a < 1) Empty
      else
        xs match {
          case Empty      => Empty
          case Cons(h, t) => Stream.cons(h(), loop(a - 1, t()))
        }
    loop(n, this)
  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def loop(a: Int, xs: Stream[A]): Stream[A] =
      if (a < 1) xs
      else
        xs match {
          case Empty      => Empty
          case Cons(_, t) => loop(a - 1, t())
        }
    loop(n, this)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}
