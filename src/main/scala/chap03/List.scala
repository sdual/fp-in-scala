package chap03

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ds: List[Double]): Double = {
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
  }

  // exercise 3.2
  def tail[A](as: List[A]): List[A] = {
     as match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  // exercise 3.3
  def setHead[A](as: List[A], rp: A): List[A] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(rp, xs)
    }
  }

  // exercise 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => Nil
      case cons if n == 0 => cons
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def dw(ls: List[A]): List[A] = {
      ls match {
        case Nil => Nil
        case Cons(x, xs) if f(x) => dw(xs)
        case xs => xs
      }
    }

    dw(l)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  // exercise 3.6
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @tailrec
    def it(ls: List[A]): List[A] = {
      ls match {
        case Nil => sys.error("init of empty list.")
        case Cons(_, Nil) => List(buf.toList: _*)
        case Cons(x, xs) =>
          buf += x
          it(xs)
      }
    }
    it(l)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, n) => n + 1)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

}

object Test extends App {
  val as = List(2, 4, 6, 8, 10, 3, 5, 8)

  println(List.length(as))

}