package chap02

class Curry {

  // exercise 2.3
  def curry[
  A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => f(a, _)
  }

  // exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

}
