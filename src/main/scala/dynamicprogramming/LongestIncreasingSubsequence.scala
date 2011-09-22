package dynamicprogramming

import math._

object LISS {

  def liss1(ls : List[Int]) = {

    val n = ls.size
    val C = Array.ofDim[Int](n + 1, n + 1)

    for (k <- 0 to n; s <- 1 to n) C(k)(s) = Int.MaxValue
    for (k <- 0 to n) C(k)(0) = Int.MinValue

    for (k <- 1 to n) {
      for (s <- 1 to n) {
        if (C(k-1)(s-1) < ls(k-1) && (ls(k-1) < C(k-1)(s))) {
          C(k)(s) = ls(k-1)
          println("here " + C(k)(s))
        }
        else {
          C(k)(s) = C(k-1)(s)
        }
      }
    }

    C(n)(n)
  }

  def liss2(ls : List[Int]) = {

    val n = ls.size
    val C = Array.ofDim[Int](n + 1)

    for (i <- 0 to n) C(i) = 1
    C(0) = 1

    for (i <- (1 to n)) {

      var m = 0
      for (j <- (0 until i)) {
        if (ls(i-1) < ls(j)) {
          m = max(m, C(j))
        }
      }
      C(i) = m + 1
    }

    println(C.toList.mkString(", "))

    C(n - 1)
  }

  def main(args : Array[String]) {

    val ls1 = List(3, 1, 2, 6, 1, 4, 7, 8)
    val ls2 = List(9, 2, 5, 3, 7, 11, 8, 10, 13, 6)

    val results1 = liss2(ls1)
    //val results2 = liss(ls2)

    println(results1)
    //println(results2)
  }
}
