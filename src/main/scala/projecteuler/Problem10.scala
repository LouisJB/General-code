package projecteuler

import java.lang.Math._

object Problem10 {

  def main(args : Array[String]) {

    def isPrime(n: Int) = (2 to (ceil(sqrt(n))).toInt) forall (n % _ != 0)

    val lim = 2000000
    var sum : Long = 2
    var j : Int = 3
    
    while (j < lim) {
      if (isPrime(j)) {
        sum += j
      }
      j = j + 1
    }

    println("sum = ")
    println(sum)

    println("getPrimeSum = ")
    getPrimeSum(lim)
  }

  def getPrimeSum(n : Int) = {
    val c = sqrt(n).toInt
    val sieve : Array[Boolean] = new Array(n+1)
    for (x <- 4 to n by 2) {
      sieve(x) = true
    }

    for (x <- 3 to c by 2) {
      if (!sieve(x)) {
        for (m <- x*x to n by 2*x) {
          sieve(m) = true
        }
      }
    }

    var sum : Long = 0

    for (x <- 2 to n) {
      if (!sieve(x)) {
        sum = sum + x
      }
    }

    println(sum)
  }
}
