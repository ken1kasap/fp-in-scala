package fpinscala.ch04

sealed trait Option[+A] {
  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None    => default
  }

  def orElse[B >: A](ob: Option[B]): Option[B] = this match {
    case None => ob
    case _    => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _               => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None            extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m =>
      mean(xs.map { x =>
        math.pow(x - m, 2)
      })
    }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      x <- a
      y <- b
    } yield f(x, y)

  // Exercise 4.4
  // List(Some(1), Some(2), Some(3)) => Some(List(1, 2, 3))
  // List(Some(1), None, Some(3)) => None
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head :: tail =>
      head flatMap { x =>
        sequence(tail) map { t =>
          x :: t
        }
      }
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head :: tail =>
      for {
        x <- head
        y <- sequence(tail)
      } yield x :: y
  }

  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil          => Some(Nil)
    case head :: tail => map2(f(head), traverse(tail)(f))(_ :: _)
  }
}
