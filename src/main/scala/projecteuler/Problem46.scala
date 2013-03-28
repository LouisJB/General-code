package projecteuler

// What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
import utils.Primes

object Problem46 {

  def main(args : Array[String]) {

    println("Goldbach false at: " + findGoldbach)
  }

  def findGoldbach() : Option[Int] =
    Stream.from(2)
      .filter(n => !Primes.isPrime(n) && (n + 1) % 2 == 0)
      .find(n => (!isGoldbach(n)))

  /*
    for (n <- Stream.from(2) if (!Primes.isPrime(n) && (n + 1) % 2 == 0)) {

      if (!isGoldbach(n)) return n
    }

    0
  }
  */

  def isGoldbach(n : Int) : Boolean =
    !(1 to n).map(x => n - (2 * x * x))
      .forall(y => y <= 1 || !Primes.isPrime(y))

  /* equivalent verbose imperative version
    for (i <- 1 to n) {

      val n = n - 2 * i * i

      if ((n > 1) && (Primes.isPrime(n)))
        return true
    }

    false
  }
  */
}
