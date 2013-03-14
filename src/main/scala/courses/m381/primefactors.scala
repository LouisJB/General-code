package courses.m381

/*
#!/bin/sh
exec scala "$0" "$@"
!#
*/

import scala.math._
import scala.annotation.tailrec

object PrimeFactors extends App {

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

  def squareFree(x : Int) : Boolean = {
    val factors = primeFactorPowers(x).toSet
    squareFree(factors)
  }
  type ~>[T, R] = PartialFunction[T, R]
  def squareFree(factors : Set[(Int, Int)]) : Boolean =
    !factors.exists{ case (_, power) => power >= 2}
  def mobius : ~>[Int, Int] = { case n if n >= 1 =>
    val factors = primeFactorPowers(n)
    val sf = squareFree(factors.toSet)
    if (sf && factors.map(_._2).sum % 2 == 0)
      1
    else if (sf)
      -1
    else
      0
  }

  def mertens(n : Int) =
    (1 to n).map(mobius).sum

  val ms = (1 to 1000).map(x => (x, mobius(x)))

  def count(ls : List[Int], n : Int) = ls.reduce((a , b) => if (b == 1) a+1 else a)
  val vs = ms.map(_._2).toList
  val ones = count(vs, 1)
  val negones = count(vs, -1)
  println("ones = " + ones + ", net ones = " + negones + ", zeroes = " + (1000 - ones - negones))
  ms.foreach(println)

  println(Primes.primeFactors(args(0).toInt))
}
