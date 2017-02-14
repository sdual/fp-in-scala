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

  // exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight2(a1, a2)((x, y) => Cons(x, y))
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

  // exercise 3.13
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(List.reverse(as), z)(f)
  }

  // exercise 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
    }
  }

  // exercise 3.13
  def foldLeft2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldRight(List.reverse(as), z)(f)
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  // exercise 3.11
  def sum3(ns: List[Int]) = foldLeft(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // exercise 3.11
  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  // exercise 3.12
  def reverse[A](ns: List[A]): List[A] = {
    foldLeft(ns, Nil: List[A])((x, y) => Cons(x, y))
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, n) => n + 1)
  }

  // exercise 3.15
  def flatten[A](as: List[List[A]]): List[A] = {
    foldRight2(as, Nil: List[A])((x, y) => append2(x, y))
  }

  // exercise 3.16
  def plusOne(as: List[Int]): List[Int] = {
    foldRight2(as, Nil: List[Int])((x, y) => Cons(x + 1, y))
  }

  // exercise 3.17
  def foreachString(as: List[Double]): List[String] = {
    List.foldRight2(as, Nil: List[String])((x, y) => Cons(x.toString, y))
  }

  // exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight2(as, Nil: List[B])((x, y) => Cons(f(x), y))
  }

  // exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight2(as, Nil: List[A]) {
      (x, y) =>
        if (f(x))
          Cons(x, y)
        else
          y
    }
  }

  // exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as){
      x =>
        if (f(x))
          List(x)
        else
          Nil
    }
  }

  // exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    flatten(
      foldRight2(as, Nil: List[List[B]]){
        (x, y) =>
          Cons(f(x), y)
      }
    )
  }

  // exercise 3.22
  def zipPlus(a1: List[Int], a2: List[Int]): List[Int] = {
    a1 match {
      case Nil => Nil
      case Cons(x, xs) => a2 match {
        case Nil => Nil
        case Cons(y, ys) => Cons(x + y, zipPlus(xs, ys))
      }
    }
  }

  // exercise 3.23
  def zipWith[A, B, C](a1: List[A], a2: List[B])(f: (A, B) => C): List[C] = {
    a1 match {
      case Nil => Nil
      case Cons(x, xs) => a2 match {
        case Nil => Nil
        case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
      }
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

}

object Test extends App {
  val as = List(2, 4, 6, 8, 10, 3, 5, 8)
  val as2 = List(20, 21)
  val as3 = List(List(1, 2), List(3, 4), List(4, 5))
  val ds = List(1.0, 4.0, 6.0)

  println(List.zipPlus(as2, as))

}
