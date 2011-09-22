package projecteuler

import utils.Maths._

object Problem45 {

  def apply(n : Int) = new Problem45(n)

  def main(args : Array[String]) {

    val p45 = Problem45(285)

    println("ans = " + p45.findNext())
  }
}

class Problem45(n : Int) {

  def findNext() : Long = {

    for (i <- n+1 to 100000000) {

      val t = tri(i)
      val p = find(t, pent)

      if (p > 0) {
        println("p " + p + " value " + t)
        val h = find(t, hex)
        if (h > 0) return t
      }
    }

    0
  }

  def find(i : Long, f : Long => Long) : Long = {

    val a = sqrtBI(BigInt(i)).toLong
    for (pn <- 300L to i) {

      val p = f(pn)
      if (p == i) return pn
      else if (p > i) return 0
    }

    0
  }

  def findPent(i : Long) : Long = {

    find(i, pent)
  }

   def findHex(i : Long) : Long = {

    find(i, hex)
  }

  def tri(n : Long) : Long = (n*(n+1))/2
  def pent(n : Long) : Long = (n*(3*n-1))/2
  def hex(n : Long) : Long = (n*(2*n-1))
}
