package projecteuler

import utils.Primes

object Problem41 {

  def main (args : Array[String]) {

    println(isPandigital(9876543210L))
    println(isPandigital(9876543219L))

    
    for (s <- 9 to 1 by -1) {
      var str = ""
      for (x <- s to 1 by -1) {
        str = str + x.toString
      }
      val n = str.toLong
      val res = find(n)
      println("Problem 41: " + res)
    }
  }

  def find(n : Long) : Long = {

    val p = Utils.permuations(n.toString.toList).map(_.mkString.toLong)
    val perms = p.sortWith(_ > _)
    var max = 0L

    perms.foreach(i => {
      if (isPandigital(i)) {
        if (Primes.isPrime(i)) {
          if (i > max) {
            max = i
            println("found " + max)
            return max
          }
        }
      } else {
        println("error " + i)
      }
    });

    max
  }

  def isPandigital(n : Long) : Boolean = {
    val s = n.toString.reverse
    for (x <- s) {
      if (s.count(_.equals(x)) > 1) return false
    }

    true
  }
}
