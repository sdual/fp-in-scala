package monad.free

import scalaz.{Free, Functor, ~>}
import scalaz.Scalaz._

trait Command[+A]
case class Cd[A](path: String, next: A) extends Command[A]
case class Ls[A](next: A) extends Command[A]
case class Done() extends Command[Nothing]

object Command {

  implicit val cf = new Functor[Command] {
    def map[A, B](c: Command[A])(f: A => B): Command[B] = c match {
      case Cd(path, next) => Cd(path, f(next))
      case Ls(next) => Ls(f(next))
      case Done() => Done()
    }
  }

  def cd(path: String): Free[Command, Unit] = Free.liftF[Command, Unit](Cd(path, Done()))
  def ls: Free[Command, Unit] = Free.liftF[Command, Unit](Ls(Done()))

  val interp = new (Command ~> Id) {
    import scala.sys.process._
    def apply[A](a: Command[A]): A = a match {
      case Cd(path, next) =>
        val cmd = s"cd $path"
        cmd.!!
        println(cmd)
        next
      case Ls(next) =>
        println("ls".!!)
        next
    }
  }
}

object TestFree extends App {

  import Command._

  val f1: Free[Command, Unit] = for {
    _ <- cd("/")
    _ <- ls
    _ <- cd("/usr/bin")
    _ <- ls
  } yield ()

  println(f1.foldMap(interp))
}
