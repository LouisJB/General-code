package utils

import scala.math.Numeric

object Maths {

  def sqrtBI(n:BigInt) = {
    def next(guess:BigInt) = (guess + n / guess) / 2
      Stream.iterate(n)(next).find(square(_) <= n).get
  }

  def square(n : BigInt) = n * n

  def modN(n: Int)(x: Int) = ((x % n) == 0)

  def add[T](a : T, b : T)(implicit num : Numeric[T]) = num.plus(a, b)

  // basic formula is n! / (n-k)!
  def permutations(n : Int, k : Int) : Long = {
    // but it can be optimised by cancelling n-k! elements out of n!
    (n-k+1 to n).foldLeft(1)((s, e) => s * e)
  }

  def combinations(n : Int, k : Int) = (1 to k).foldLeft(1)((s, e) => (s * (n - e + 1)) / e)


  def gcd(a : Int, b : Int) : Int = if (b == 0) a else gcd(b, a % b)
}
