package fpinscala.ch02

import scala.annotation.tailrec

object FibonacciNumber {
  // exercise 2.1 n番目のフィボナッチ数を取得する再帰関数
  def fib(n: Int): Int = {

    @tailrec
    def _fib(c: Int, a: Int, b: Int): Int = {
      if (c == 0) a
      else _fib(c - 1, b, a + b)
    }

    if (n < 1) 0
    else _fib(n - 1, 0, 1)
  }
}
