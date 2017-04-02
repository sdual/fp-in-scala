package stackless

sealed trait Trampoline[+A] {
  final def runT: A = resume match {
    case Right(a) => a
    case Left(k) => k().runT
  }

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = {
    this match {
      case a FlatMap g => FlatMap(a, (x: Any) => g(x) flatMap f)
      case x => FlatMap(x, f)
    }
  }

  def map[B](f: A => B): Trampoline[B] = {
    flatMap(a => Done(f(a)))
  }

  final def resume: Either[() => Trampoline[A], A] =
    this match {
      case Done(a) => Right(a)
      case More(k) => Left(k)
      case a FlatMap f => a match {
        case Done(a) => f(a).resume
        case More(k) => Left(() => k() flatMap f)
        case b FlatMap g => b.flatMap((x: Any) => g(x) flatMap f).resume
      }
    }

}

case class Done[A](a: A) extends Trampoline[A]

case class More[A](k: () => Trampoline[A]) extends Trampoline[A]

case class FlatMap[A, B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]

object Factorial extends App {

  println(factorial(10000).runT)

  def factorial(n: BigInt): Trampoline[BigInt] = {
    if (n <= 1)
      Done(1)
    else
      More(() => factorial(n - 1).map(n * _))
  }

}
