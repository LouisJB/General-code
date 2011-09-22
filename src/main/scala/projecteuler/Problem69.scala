package projecteuler

import utils.Primes._
import scala.math._
import utils.Timer._

/**
 * Created by IntelliJ IDEA.
 * User: chillipower_uk
 * Date: 06/03/2011
 * Time: 08:42
 * To change this template use File | Settings | File Templates.
 */

object Problem69 {

  def gcd(a : Int, b : Int) : Int = if (b == 0) a else gcd(b, a % b)
  def totient1(n : Int) = (0 until n).filter(x => gcd(x, n) == 1).size

  def totient(start : Int) : Int =
    primeFactorMultiplicity(start).foldLeft(1) { (r, f) =>
      f match { case (p, m) => r * (p - 1) * pow(p, m - 1).toInt }
  }

  var max = 0.0

  def main(args : Array[String]) {

    time(t => println("took " + t + " ms")) {
      val v = (1000000 to 2 by -2)
          .foreach(x => {
        val t = totient(x)
        if ((x / t) >= floor(max)) {
          val m = x.toDouble / t.toDouble;
          if (m >= max) {
            max = m
            println("x = " + x + ", " + max)
          }
        }
      }       )
    }

    //)) max Ordering[Double].on[(_,Double)](_._2)
  }
}
