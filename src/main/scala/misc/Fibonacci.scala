package misc

import scala.collection.mutable.{Map => MMap}
import annotation.tailrec

object Fibonacci {

  def fibr(n : Int) = {
    @tailrec
    def fibtr(x : Int, a : Int, b : Int) : Int = x match { case 0 => a; case _ => fibtr(x - 1, a + b, a) }
    fibtr(n, 0, 1)
  }

  def fib(n : Int) = {
    var count = 0

    def fastFib(m : Int, memo : MMap[Int, Long]) : Long = {
      if (!memo.contains(m)) {
        count = count + 1
        val x = fastFib(m-1, memo) + fastFib(m-2, memo)
        memo.put(m, x)
        x
      }
      else memo(m)
    }

    val memo = MMap[Int, Long](
      0 -> 1,
      1 -> 2)

    val x = fastFib(n, memo)
    println("No of calls = " + count)
    x
  }

  def main(args : Array[String]) {

    val x = fib(50)

    println("fib(100) = " + x)

    val fac = Memoize1.Y(facRec)

    for (k <- 200 to 0 by -1)
      println(fac(k))
  }

  def facRec(n: BigInt, f: BigInt => BigInt): BigInt = {
    if (n == 0) 1
    else n*f(n - 1)
  }
}


class Memoize1[-T, +R](f: T => R) extends (T => R) {

  import scala.collection.mutable

  private[this] val vals = mutable.Map.empty[T, R]

  def apply(x: T): R = {

    if (vals.contains(x)) {
      vals(x)
    }
    else {
      val y = f(x)
      vals + ((x, y))
      y
    }
  }
}

object Memoize1 {
  def apply[T, R](f: T => R) = new Memoize1(f)

  def Y[T, R](f: (T, T => R) => R) = {

    var yf: T => R = null
    yf = Memoize1(f(_, yf(_)))
    yf
  }
}
