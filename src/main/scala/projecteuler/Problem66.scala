package projecteuler

import math._

// x2 – Dy2 = 1
object Problem66 {

  def isSquare(n : Int) = { val r = sqrt(n); (r * r == n) }

  def main(args : Array[String]) {
    var dMax = 0
    var xMax = 0
    (2 to 100).foreach { d =>
      if (!isSquare(d)) {
        (1 to 100000).find { x =>
          (1 to 50000).find { y =>
            val a = x*x - d*y*y
            //println("a = " + a  + ", d = " + d + ", x = " + x + ", y = " + y)
            if (a == 1) {
              if (x > xMax) {
                xMax = x
                dMax = d
                println("dMax = " + dMax + ", at x = " + x + ", y = " + y)
              }
              true
            }
            else false
          }.isDefined
        }
      }
    }
  }
}
