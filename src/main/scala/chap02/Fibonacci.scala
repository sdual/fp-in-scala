package chap02

import scala.annotation.tailrec

object Fibonacci extends App {
  for {
    i <- 0 to 10
  } yield println(fibonacci(i))

  def fibonacci(n: Int): Int = {
    @tailrec
    def fib(fN: Int, fNext: Int, i: Int): Int = {
      if (i == n) fN
      else fib(fNext, fN + fNext, i + 1)
    }

    fib(1, 1, 0)
  }

}
