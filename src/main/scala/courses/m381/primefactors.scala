#!/bin/sh
exec scala "$0" "$@"
!#

import scala.math._
import scala.annotation.tailrec

object Primes {

  def pSieve(s : Stream[Int]) : Stream[Int] = { s.head #:: pSieve(s.filter(_ % s.head > 0)) }

  val primeStream = pSieve(Stream.from(2))

  // return a list of prime factors...
  def primeFactors(n : Int) : List[Int] = {

    var primeFactors = List[Int]()

    var x = n
    var i = 1
    for (i <- 2 to (x / i) if x > 1) {
      while (x % i == 0) {
        primeFactors = i :: primeFactors
        x = x / i
      }
    }

    primeFactors.reverse
  }

  def primeFactorPowers(n : Int) : List[(Int, Int)] =
    primeFactors(n).groupBy(identity).map{case (x, y) => (x, y.length)}.toList.reverse
  
  def divisorLists(xs : List[(Int, Int)], ys : List[Int]) : List[List[Int]] = xs match {
    case (a, b) :: zs => if (b > 0) divisorLists((a, b - 1) :: zs, a :: ys) ::: divisorLists(zs, ys) else divisorLists(zs, ys)
    case _ => List(ys)
  }

  def divisors(n : Int) = divisorLists(primeFactorPowers(n), List()).filter(_.size > 0).map(e => e.reduceLeft(_ * _))
}

println(Primes.primeFactors(args(0).toInt))


