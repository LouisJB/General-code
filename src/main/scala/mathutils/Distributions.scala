package mathutils

import math._

object Binomial {

  // Binomial Coefficient C(n, k)
  def C(n : Int, k : Int) = (1 to k).foldLeft(1)((s, e) => (s * (n - e + 1)) / e)

  // probability mass function, probability of k success in n trials
  // with probability p of each trial being a success (independent)
  def f(k : Int, n : Int, p : Double) : Double = {
    C(n, k) * pow(p, k) * pow(1-p, n-k)
  }

  def E(n : Int, p : Double) : Double  = n * p
  def varX(n : Int, p : Double) : Double = n * p * (1 - p)

  def cdf(x : Double, n : Int, p : Double) : Double =
    (0 to x.toInt).map(i => (C(n, i) * pow (p, i) * pow (1-p, n-i))).sum
}