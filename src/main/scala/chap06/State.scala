package chap06

trait RNG {
  def nextInt: (Int, RNG)
}
