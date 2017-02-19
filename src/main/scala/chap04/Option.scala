package chap04

import scala.annotation.tailrec

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(a) => f(a)
      case None => None
    }
  }
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(a) => a
      case None => default
    }
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case t @ Some(a) => t
      case None => ob
    }
  }
  def filter(f: A => Boolean): Option[A] = {
    this match {
      case t @ Some(a) if f(a) => t
      case _ => None
    }
  }

  // exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def seq(acc: List[A], b: List[Option[A]]): Option[List[A]] = {
      b match {
        case Some(x) :: xs => seq(x :: acc, xs)
        case _ => None
      }
    }

    seq(Nil ,a)
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object OptionTtest extends App {

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val abs0: Option[Double] => Option[Double] = lift(math.abs)

}
