package projecteuler

import utils.Maths._

object Problem73 {

  // http://www.cut-the-knot.org/blue/Stern.shtml
  def main(args : Array[String]) {

    case class Fraction(n : Int, d : Int) {
      def median(x : Fraction) = Fraction(n + x.n, d + x.d)
      override def toString() = "" + n + "/" + d
    }

    def f(a : Fraction, b : Fraction, z : Int) : List[Fraction] =
      if ((a median b).d <= z) (a median b) :: f(a median b, b, z) ::: f(a, a median b, z)
      else Nil

    val r = f(Fraction(1, 3), Fraction(1, 2), 12000)

    println("problem 73 answer = " + r.size)


    var count = 0

    for (d <- 5 to 12000) { val bmax = d/2; for (a <- (d/3+1 to bmax))  { if (gcd(a, d) == 1) { count = count + 1 }  } }

    println("count = " + count)

    val c = (5 to 12000).map(d => { val bmax = d/2; (d/3+1 to bmax).map(a => (gcd(a, d) == 1))}).flatten.filter(_ == true).size

    println("count2 = " + c)
  }
}
