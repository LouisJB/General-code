package courses.m381.m343

import math._

import courses.m381.m343.Probability._

case class MMNQueue(
  n : Int,      // number of servers
  λ : Double,   // arrival rate
  ε : Double    // service rate
) {

  def ρ : Double = λ / (n * ε)

  def K = (0 to n).map(x => {
    val base = pow(n * ρ, x) / (x!)
    if (x < n)
      base
    else
      base / (1 - ρ)
  }).sum

  def p(x : Int) : Double = {
    if (x < n)
      (1.0 / K) * (pow(n, x) / (x!)) * (pow(ρ, x))
    else
      (1.0 / K) * (pow(n, n) / (n!)) * (pow(ρ, x))
  }

  def plte(x : Int) = (0 to x).map(p _).sum

  override def toString =
    "n %s λ %s ε %s ρ %s K %s".format(n, λ, ε, ρ, K)
}

object MMNQueue {
  def main(args : Array[String]) {
    def printQStats(q : MMNQueue) {
      println("q = " + q)
      println("p(n) = " + (0 to 10).map(x => (x, q.p(x))).mkString(", "))
      println("plte = " + (0 to 10).map(x => (x, q.plte(x))).mkString(", "))
    }

    val q1 = MMNQueue(4, 1.0, 1.0/3)
    printQStats(q1)

    val q2 = MMNQueue(3, 40.0, 20.0)
    printQStats(q2)
  }
}
