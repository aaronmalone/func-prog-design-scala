package calculator

import java.lang.Math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] =
    Signal(b() * b() - 4 * a() * c())

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    def f(aa: Double, bb: Double, dd: Double): Set[Double] = {
      if (dd < 0) Set.empty
      else {
        val s = sqrt(dd)
        val twoA = 2 * aa
        Set((-bb + s) / twoA, (-bb - s) / twoA)
      }
    }
    Signal(f(a(), b(), delta()))
  }
}
