package fpinscala.ch05

import Stream._
import scala.annotation.tailrec

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  // Exercise 5.6
  def headOption2: Option[A] = foldRight(Option.empty[A]) { (a, _) =>
    Some(a)
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

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => Empty
  }

  // Exercise 5.5
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A]) { (a, b) =>
    if (p(a)) cons(a, b) else empty
  }

  @tailrec
  final def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false
  }

  def foldRight[B](z: => B)(f: (A, B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (h, t) =>
      if (f(h)) cons(h, t)
      else t
    }
  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](a: A => Stream[B]): Stream[B] = foldRight(empty[B]) { (h, t) =>
    a(h) append t
  }

  def existsFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty      => true
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _          => false
  }

  // forAll with foldRight
  def forAll2(p: A => Boolean): Boolean = foldRight(true) { (a, b) =>
    p(a) && b
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
