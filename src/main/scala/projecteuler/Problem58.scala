package projecteuler

import utils.Primes._

// answer 26241
object Problem58 {

  def ul(x : Int) = x*x - 2*x + 2 // x * x - x - (x - 1) + 1
  def ur(x : Int) = x*x - 3*x + 3// x*x - x - (2 * (x - 1)) + 1
  def lr(x : Int) = x*x
  def ll(x : Int) = x*x - x + 1

  def main(args : Array[String]) {

    var p = 0
    var n = 1

    (1 to 100000 by 2).map(x => {
      if (isPrime(ur(x))) p = p + 1
      if (isPrime(ul(x))) p = p + 1
      if (isPrime(lr(x))) p = p + 1
      if (isPrime(ll(x))) p = p + 1

      val r : Double = (p.doubleValue / n.doubleValue)
      println("x = " + x + ", p = " + p + " n = " + n + ", r = " + r)

      if (x > 1 && r < 0.1) {
        println("x = " + x)
        return;
      }

      n = n  + 4
    })
  }
}
