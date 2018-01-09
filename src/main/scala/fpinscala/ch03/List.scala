package fpinscala.ch03

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil                             extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](list: List[A]): List[A] =
    list match {
      case Cons(_, xs) => xs
      case Nil         => Nil
    }

  // Exercise 3.3
  def setHead[A](x: A, xs: List[A]): List[A] =
    xs match {
      case Cons(_, tail) => Cons(x, tail)
      case Nil           => Nil
    }

  // Exercise 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil => l
        case _   => drop(tail(l), n - 1)
      }

}
