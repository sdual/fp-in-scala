package monad.free

import scalaz.Functor

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

}
