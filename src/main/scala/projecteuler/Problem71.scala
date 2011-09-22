package projecteuler

import Utils._


object Problem71 {

  def main(args : Array[String]) {

    val x = {
      def f(a : Int, b : Int, c : Int, d : Int) : Int = if (b + d <= 1000000) f(a + c, b + d, c, d) else a
      f(2, 5, 3,7)
    }

    println("answer = " + x)


    // y-combinator version - does stack overflow!
    val x2 = fix[(Int, Int, Int, Int), Int](f => a =>
      if (a._2 + a._4 <= 1000000) f(a._1 + a._3, a._2 + a._4, a._3, a._4) else a._1)((2, 5, 3, 7))


    // incidental, triangle numbers using y-comb fix
    val tri = fix[Int, Int](f => a => if (a == 0) 0 else a + f(a - 1))

    (1 to 10) map tri
  }
}
