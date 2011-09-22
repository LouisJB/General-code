package projecteuler

object Problem57 {

  def gcd(a : BigInt, b : BigInt) : BigInt =
    if (b == 0) a else gcd(b, a % b)

  def f(n : BigInt) : (BigInt, BigInt) =
    if (n == 1) (1, 2) else {
      val x = f(n-1)
      val x2 = (x._2, x._2 * 2 + x._1)
      val g = gcd(x._1, x._2)
      if (g == 1) x2 else (x2._1/g, x2._2/g)
    }

  def f2(n : BigInt) = {
    val x = f(n)
    (x._1 + x._2, x._2)
  }

  def solve(a : Int) =
    (1 to a).filter(n => {
      val x= f2(n)
      x._1.toString.size > x._2.toString.size
    }).size

  def main(args : Array[String]) = {
    println("answer = " + solve(1000))
  }
}