package monad.free

import scalaz.Free
import scalaz.Scalaz._

object TestFreeMethod extends App {
  //println(Free.point[List, Int](1))
  //println(Free.suspend(Free.point[List, Int](1)))
  //println(Free.liftF(List(1)))

  val pointInt: Int => Free[Id, Int] = Free.point[Id, Int](_)
  val suspendInt: Int => Free[Id, Int] = n => Free.suspend[Id, Int](pointInt(n))

  //println(pointInt(1).map(_ + 1).resume)
  println(pointInt(1).map(_ + 1).flatMap(suspendInt).map(_ + 1).resume.fold(_.resume, identity))
}
