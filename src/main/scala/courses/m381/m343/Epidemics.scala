package courses.m381.m343

import math._

case class SimpleEpidemic(b : Double, n : Int, y0 : Int) {
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
case class GeneralEpidemic(β : Double, γ : Double, n : Int, y0 : Int, x0 : Int) {
  def ρ : Double = (n * γ) / β // the epidemic parameter

  def yMax = y0 + x0 - ρ - ρ * log(x0 / ρ) // max simultaneous infectives
}

object Epidemics {
  def main(args : Array[String]) {

    val se1 = SimpleEpidemic(0.18, 6, 3)

    println("betas are: " + se1.betas.mkString(", "))
    println("times are: " + se1.times.mkString(", "))
    println("waiting times are: " + se1.waitingTimes.mkString(", "))
    println("waitingTimeVars are: " + se1.waitingTimeVars.mkString(", "))
    println("waitingTimeStdDevs are: " + se1.waitingTimesStdDevs.mkString(", "))


    val ge1 = GeneralEpidemic(9.0, 4.0, 27, 2, 26)

    val yMax = ge1.yMax
    println("yMax: " + yMax + ", ρ = " + ge1.ρ)
  }
}
