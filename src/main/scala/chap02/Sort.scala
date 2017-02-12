package chap02

import scala.annotation.tailrec

class Sort {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def isSt(index: Int): Boolean = {
      if (index >= as.length)
        true
      else if (!ordered(as(index - 1), as(index)))
        false
      else
        isSt(index + 1)
    }

    isSt(1)
  }

}

object Test extends App {
  val s = new Sort
  val a = Array(1,2,5,4)
  def ordered(a0: Int, a1: Int): Boolean = {
    a0 < a1
  }
  println(s.isSorted(a, ordered))
}
