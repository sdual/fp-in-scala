package monad.free

import scalaz.Scalaz._
import scalaz.{Free, Functor, ~>}

sealed trait LogF[+A]

object LogF {

  case class Debug[A](msg: String, o: A) extends LogF[A]
  case class Info[A](msg: String, o: A) extends LogF[A]
  case class Warn[A](msg: String, o: A) extends LogF[A]
  case class Error[A](msg: String, o: A) extends LogF[A]

  implicit def logFFunctor[B]: Functor[LogF] = new Functor[LogF] {
    override def map[A, B](fa: LogF[A])(f: A => B): LogF[B] = {
      fa match {
        case Debug(msg, a) => Debug(msg, f(a))
        case Info(msg, a)  => Info(msg, f(a))
        case Warn(msg, a)  => Warn(msg, f(a))
        case Error(msg, a) => Error(msg, f(a))
      }
    }
  }

}

object Println {

  import LogF._

  type Log[A] = Free[LogF, A]

  private def write(prefix: String, msg: String): Unit = {
    println(s"[$prefix] $msg")
  }

  private def debug(msg: String): Unit = {
    println(s"[debug] $msg")
  }

  private def info(msg: String): Unit = {
    println(s"[info] $msg")
  }

  private def warn(msg: String): Unit = {
    println(s"[warn] $msg")
  }

  private def error(msg: String): Unit = {
    println(s"[error] $msg")
  }

  private val exe: LogF ~> Id = new (LogF ~> Id) {
    def apply[B](l: LogF[B]): B = l match {
      case Debug(msg, a) => debug(msg); a
      case Info(msg, a) => info(msg); a
      case Warn(msg, a) => warn(msg); a
      case Error(msg, a) => error(msg); a
    }
  }

  def apply[A](log: Log[A]): A = {
    log.runM(exe.apply[Log[A]])
  }

  implicit def logFToFree[A](logf: LogF[A]): Free[LogF, A] = {
    ???
  }

}

object LogFTest extends App {

  import Println._

  val program: Free[LogF, Unit] = {
    for {
      a <-
    }
  }

}
