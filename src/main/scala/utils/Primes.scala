package utils

import java.lang.Math._
//import scala.mathutils._

import utils.Maths._


object Primes {

  // prime test using prime stream
  def isPrime(n : Int): Boolean =
    (n > 1) && (primeStream takeWhile(_ <= sqrt(n)) forall(n % _ != 0 ))

  val primeStream = Stream.cons(2, Stream.from(3, 2) filter(x => isPrime(x)))


  def primeFactorMultiplicity(start: Int) : Map[Int,Int] = {
    def factorCount(n: Int, p: Int): (Int,Int) =
      if (n % p != 0) (0, n)
      else factorCount(n / p, p) match { case (c, d) => (c + 1, d) }
    def factorsR(n: Int, ps: Stream[Int]): Map[Int, Int] =
      if (n == 1) Map()
      else if (isPrime(n)) Map(n -> 1)
      else {
        val nps = ps.dropWhile(n % _ != 0)
        val (count, dividend) = factorCount(n, nps.head)
        Map(nps.head -> count) ++ factorsR(dividend, nps.tail)
      }
    factorsR(start, primes)
  }

  // This also lets us change primeFactors.
  def primeFactors(start: Int) : List[Int] =
    primeFactorMultiplicity(start) flatMap { v => List.make(v._2, v._1) } toList


  // prime sieve; use instead of the test or stream when you know the
  // upper bound of the primes you will need.
  // en.wikipedia.org/wiki/Sieve_of_Eratosthenes
  // this is the genuine sieve algorithm; see www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
  // for a discussion of impostors
  def primesBelow(limit: Int): List[Int] = {
    val sieve = Array.fill(limit)(true)
    sieve(1) = false
    for{i <- 2 to math.sqrt(limit).toInt
        if sieve(i)
        j <- i * 2 until(limit, i)}
      sieve(j) = false
    (2 until limit).filter(sieve(_)).toList
  }

  // primality test using a sieve that doubles in size if we ask for a number larger than the current
  // sieve.  so unlike primesBelow, this doesn't require committing in advance to a ceiling on the
  // amount of primes you will need.
  var sieve = Array(false, false, true, true)
  def isSievedPrime(n: Int): Boolean = {
    while(n >= sieve.size) {
      val newSieve = Array.fill(sieve.size * 2)(true)
      Array.copy(sieve, 0, newSieve, 0, sieve.size)
      for{i <- 2 to sqrt(newSieve.size).toInt
          if newSieve(i)
          // start at least multiple of i at or above sieve.size
          j <- i * ((sieve.size - 1) / i + 1) until newSieve.size by i}
        newSieve(j) = false
      sieve = newSieve
    }
    sieve(n)
  }

  // stream of cached sieved primes
  val primes: Stream[Int] = Stream.from(2).filter(isSievedPrime)


  def isPrime(n: Long) = {
    if (n <= 1) false
    else if (n == 2) true
    else (2L to (ceil(sqrt(n))).toLong) forall (n % _ != 0L)
  }

  def isPrimeF(n : BigInt) = {
    if (n < BigInt(2)) false
    else if ((n==BigInt(2)) || (n==BigInt(3)) || (n==BigInt(5)) || (n==BigInt(7))) true
    else if ((n==BigInt(4)) || (n==BigInt(6)) || (n==BigInt(8)) || (n==BigInt(9))) false
    else (BigInt(2) to sqrtBI(n)) forall (n % _ != BigInt(0))
  }

  val primesMap = scala.collection.mutable.Map[BigInt, Boolean]()

  def isPrime(n : BigInt) : Boolean = {
    if (!primesMap.contains(n)) {
      val x = isPrimeF(n)

      if (n < 1000000) {
        primesMap.put(n, x)
      }
      x
    }
    else {
      //println("from hash")
      primesMap.getOrElse(n, false)
    }
  }

  // return a list of prime factors...
  def getPrimeFactors(n : Long) : List[Long] = {

    var primeFactors = List[Long]()

    var x = n
    var i = 1
    for (i <- 2L to (x / i) if x > 1) {
      while (x % i == 0) {
        primeFactors = i :: primeFactors
        x = x / i
      }
    }

    primeFactors.reverse
  }

  // return a list of prime factors...
  // as tuples of (value, count) to represt p^n collapse of the prime factors
  def getPrimeFactorsPowered(n : Long) : List[(Long, Int)] =
    getPrimeFactors(n).groupBy(identity).map{case (x, y) => (x, y.length)}.toList

  def f(xs : List[(Int, Int)], ys : List[Int]) : List[List[Int]] = xs match {
    case (a, b) :: zs => if (b > 0) f((a, b - 1) :: zs, a :: ys) ::: f(zs, ys) else f(zs, ys)
    case _ => List(ys)
  }
}
