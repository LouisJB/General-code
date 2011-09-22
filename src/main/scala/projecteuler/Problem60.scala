package projecteuler

import utils.Primes._
import annotation.tailrec

object Problem60 {

  val primeStream = primes
  val primesList = primeStream.take(2000000).toList
  val maxCached = primesList.last

  val pMap = scala.collection.mutable.Set[Long]() ++ primesList.filter(_ != 5)

  val primesConcat = scala.collection.mutable.Map[(Short, Short), Boolean]()

  def main(args : Array[String]) {

    import utils.Timer._

    val ls = List[Short]()

    println("primesList, max value = " + maxCached)

    time(r => println("Took " + r + "ms to run")) {
      find(5, primesList.take(1100).map(_.toShort), ls)
    }
  }

  //@tailrec
  def find(n : Int, ps : List[Short], ls : List[Short]) : List[Short] = {

    if (ls.size >= n) {
      println("Result = { " + ls.mkString(", ") + " }, sum = " + ls.sum)
      return ls
    }

    if (ps.size == 0 && n >= 4) {
      println("Checked with {" + ls.mkString(", ") + "}")
      return ls
    }

    val p = ps.head

    if (checkPrimes(p, ls)) {
      val r = find(n, ps.tail, p :: ls)
      if (r.size >= n) return r
      find(n, ps.tail, ls)
    }
    else {
      find(n, ps.tail, ls)
    }
  }

  def checkPrimes(p : Short, ls : List[Short]) =
    !ls.exists(n => notPrime(p, n))

  def notPrime(a : Short, b : Short) : Boolean = {
    val t = (a, b) //if (a >= b) (a, b) else (b, a)
    primesConcat.get(t) match {
      case Some(r) => r
      case None => {
        val r = !isPrime2((a.toString + b.toString).toLong) || !isPrime((b.toString + a.toString).toLong)
        primesConcat(t) = r
        r
      }
    }
  }

  def isPrime2(x : Long) = {
    if (pMap.contains(x)) {
      true
    }
    else {
      if (isPrime(x)) {
        pMap += x
        true
      }
      else false
    }
  }
}

