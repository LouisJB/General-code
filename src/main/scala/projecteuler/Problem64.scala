package projecteuler

import math._
import annotation.tailrec

object Problem64 extends App {

  def period(n : Int) : Int = {
    val a0 = sqrt(n).toInt
    if (a0 * a0 == n) 0 else {
      @tailrec
      def prec(m : Int, d : Int, a : Int, p : Int) : Int = {
        if (a == 2 * a0) p
        else {
          val m1 = d * a - m
          val d1 = (n - m1 * m1) / d
          val a1 = (a0 + m1) / d1
          prec(m1, d1, a1, p + 1)
        }
      }
      prec(0, 1, a0, 0)
    }
  }

  val r = (2 to 10000).map(period).filter(_ % 2 == 1).size
  
  println("Ans: " + r)
}
