package projecteuler

import java.lang.Math._
import utils._

// n² + an + b, where |a| < 1000 and |b| < 1000
object Problem27 {

  def main(args : Array[String]) {
    findQuadratic(999)
  }

  def findQuadratic(i : Int) {
    //println("test = " + checkPrimes(n => n*n - 61 * n + 971))
    //return
    println("test = " + checkPrimes(x => x*x + x + 41))
    println("test = " + checkPrimes(x => x*x - 79 * x + 1601))


    var max = 0
    for (a <- -i to i) {
      for (b <- -i to i) {
        if (Primes.isPrime(abs(b))) {
          val quadraticFn = (n : Long) => {
            BigInt((n * n) + (a * n) + b)
          }

          val x = checkPrimes(quadraticFn)

          if (x > max) {
            max = x
            println("Max at a = " + a + ", b = " + b + ", n = " + x
                    + ", product = " + (a * b))
          }
        }
      }
      //println("a = " + a)
    }
  }

  def checkPrimes(fn : Long => BigInt) : Int = {

    var x = 0
    for (n <- 0 to 1000) {
      val z = fn(n)
      val r = Primes.isPrime(z.abs)
      if (r == false) return x
      x = x + 1
      //println(z)
    }
    x
  }
}
