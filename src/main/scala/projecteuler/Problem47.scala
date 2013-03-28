package projecteuler

import utils.Primes._


object Problem47 {

  def main(args : Array[String]) {

    val pf1 = getPrimeFactors(14)
    val pf2 = getPrimeFactors(15)

    val ts = List(2,2,2,3,5,5,7).groupBy(identity).map{case (x,y) => (x, y.length)}.toList
    println(ts.mkString(","))

    println("pf1 = " + pf1.mkString(","))
    println("pf2 = " + pf2.mkString(","))
    
    val x = findPf(3, 3)
    println("n = " + x)

    val y = findPf(4, 4)
    println("y = " + y)
  }

  def findPf(p : Int, count : Int) : Int = {
    var c = 0

    for (n <- Stream.from(2)) {
    
      val pf = getPrimeFactors(n)
      val s = pf.distinct.size

      if (s == p) {
        c = c + 1
        println("n = " + n + ", pf = " + pf.mkString(",") + ", c = " + c)
        if (c >= count) {
          return n - count + 1
        }
      }
      else {
        c = 0
      }
    }

    0
  }
}
