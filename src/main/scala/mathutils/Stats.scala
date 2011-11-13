package mathutils

import math._

object Utils {
  import Numeric.Implicits._
  def average[T : Numeric](ls : List[T]) : Double = ls.sum.toDouble / ls.size.toDouble
  def stdDev[T : Numeric](ls : List[T]) : Double = pow(ls.map(x => pow((x.toDouble - average(ls)), 2)).sum / ls.size, 0.5)
  def variance[T : Numeric](ls : List[T]) = ls.map(x => pow((x.toDouble - average(ls)), 2)).sum / ls.size
  def continuousInterest(r : Double, t : Double) = exp(r * t)
}
