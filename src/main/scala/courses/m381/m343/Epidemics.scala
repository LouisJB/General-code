package courses.m381.m343

import math._
import annotation.tailrec

case class NumericalIntegrator(stepSize : Int = 1000)(fn : Double => Double) {
  import Itr._
  def definiteIntegral(a : Double, b : Double) : Double = {
    val steps : List[Double] = (a, b).evenly(stepSize)
    val stepPairs = steps.zip(steps.tail)
    val r1 = stepPairs.map(x => {
      (x._2 - x._1) * (fn(x._1) + fn(x._2)) / 2.0
    }).sum
    val basicTrapezium = (b - a) * (fn(a) + fn(b)) / 2.0
    println("%s, %s".format(r1, basicTrapezium))
    r1
  }
}
case object NumericalIntegrator {
  def main(args : Array[String]) = {
    val fn : (Double) => Double = (x : Double) => pow((1.0 - (x / 100.0)), 3.0)
    val ni1 = NumericalIntegrator()(fn)
    val di1 = ni1.definiteIntegral(0.0, 100.0)
    // should be [ -100 * 1/4 ( 1 - x/100)^5  ]0-100 = (-25 * (1 - 1) = 0 ) - -25 = +25
    println("di1 = " + di1)
  }
}

case class StochasticSimpleEpidemic(b : Double, n : Int, y0 : Int) {
  def by(y : Int) = (b * y * (n+1-y) ) / n
  def betas = (y0 to n).map(y => (y, by(y)))
  def times = betas.map(t => (t._1, 1.0/t._2))
  def waitingTimes = (y0 to n).map(y => (y, (y0 to y).map(x => 1.0/by(x)).sum))
  def waitingTimeVars = (y0 to n).map(y => (y, (y0 to y).map(x => 1.0/pow(by(x), 2.0)).sum))
  def waitingTimesStdDevs = waitingTimeVars.map(v => (v._1, sqrt(v._2)))
}

/**
 *
 * @param β rate of contact
 * @param γ rate of removal
 * @param n total size - 1
 * @param y0 initial infectives
 * @param x0 initial susceptibles
 */
case class DeterministicGeneralEpidemic(β : Double, γ : Double, n : Int, y0 : Int, x0 : Int) {
  def ρ : Double = (n * γ) / β // the epidemic parameter

  // max y simultaneous infectives
  def yMax = y0 + x0 - ρ - ρ * log(x0 / ρ)
  // survivor x at ∞ populuation
  def xAtInf(xj : Double, maxDepth : Int, delta : Double = 1E-06) : Double = {
    if (maxDepth == 0)
      xj
    else {
      val nextVal = x0 * exp((xj - (x0 + y0)) / ρ)
      if (abs(xj - nextVal) < delta) {
        println("depth " + maxDepth)
        nextVal
      }
      else
        xAtInf(nextVal, maxDepth - 1, delta)
    }
  }
}

object Epidemics {
  def main(args : Array[String]) {
    val sse1 = StochasticSimpleEpidemic(0.18, 6, 3)

    println("betas are: " + sse1.betas.mkString(", "))
    println("times are: " + sse1.times.mkString(", "))
    println("waiting times are: " + sse1.waitingTimes.mkString(", "))
    println("waitingTimeVars are: " + sse1.waitingTimeVars.mkString(", "))
    println("waitingTimeStdDevs are: " + sse1.waitingTimesStdDevs.mkString(", "))

    val dge1 = DeterministicGeneralEpidemic(9.0, 4.0, 27, 2, 26)

    val yMax = dge1.yMax
    println("yMax: " + yMax + ", ρ = " + dge1.ρ)

    val xInf = dge1.xAtInf(0, 100)
    println("dge1 xAtInf: " + xInf)
  }
}


case class Itr(a : Double, b : Double, n : Int = 1000) {
  final def mapEvenly[T](n : Int = 1000)(fn : Double => T) = {
    val step = (b-a) / n.toDouble
    itr(a, b, step, fn)
  }
  final def evenly(n : Int = 1000) : List[Double] = {
    val step = (b-a) / n.toDouble
    itr(a, b, step, (x : Double) => x)
  }
  //@tailrec
  private def itr[T](x : Double, y : Double, step : Double, fn : Double => T) : List[T] = {
    val v = fn(x)
    if ((y-x) <= 0) return v :: Nil
    v :: itr(min(x+step, y), y, step, fn)
  }
}
object Itr {
  implicit def toItr(range : (Double, Double)) : Itr= Itr(range._1, range._2)
  def main(args : Array[String]) {
    val r = (1.0, 10.0).mapEvenly(10) { x =>
      println(x); x
    }
    println(r.mkString(", "))
  }
}
