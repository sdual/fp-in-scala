package monad.free

import scalaz.{Free, Functor}

sealed trait CharToy[+Next]

object CharToy {

  case class CharOutput[Next](a: Char, next: Next) extends CharToy[Next]
  case class CharBell[Next](next: Next) extends CharToy[Next]
  case class CharDone() extends CharToy[Nothing]

  implicit val charToyFunctor: Functor[CharToy] = new Functor[CharToy] {
    override def map[A, B](fa: CharToy[A])(f: A => B) = fa match {
      case o: CharOutput[A] => CharOutput(o.a ,f(o.next))
      case b: CharBell[A]   => CharBell(f(b.next))
      case CharDone()       => CharDone()
    }
  }

  def output(a: Char): Free[CharToy, Unit] = {
    Free.liftF[CharToy, Unit](CharOutput(a, CharDone()))
  }

  def bell: Free[CharToy, Unit] = {
    Free.liftF[CharToy, Unit](CharBell(CharDone()))
  }

  def done: Free[CharToy, Unit] = {
    Free.liftF[CharToy, Unit](CharDone())
  }

}

object CharToyTest extends App {

  import CharToy._

  val c1: Free[CharToy, Unit] = for {
    _ <- output('A')
    _ <- bell
    _ <- done
  } yield ()

  println(c1)

}
