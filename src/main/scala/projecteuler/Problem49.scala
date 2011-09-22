package projecteuler

import utils.Primes._

object Problem49 {

  def main(args : Array[String]) {

    println("test = " + same(123, 213, 321))

    findSeq()
  }

  def findSeq() = {

    for (a <- 1000 to 9999 if (isPrime(a))) {
      for (b <- 2 to 10000 - a) {

        val x = a + b
        val y = a + 2*b

        if (isPrime(x) && isPrime(y)) {

          if (same(a, x, y)) {
            println(a)
            println(x)
            println(y)

            println(a.toString + x.toString + y.toString)
          }
        }
      }
    }
  }

  def same(a : Int, b : Int, c : Int) = {

    val as : List[Int] = a.toString.toList.map(_.toString.toInt)
    val bs = b.toString.toList.map(_.toString.toInt)
    val cs = c.toString.toList.map(_.toString.toInt)

    val as1 = as.sortWith(_ > _)
    val bs1 = bs.sortWith(_ > _)
    val cs1 = cs.sortWith(_ > _)

    //val r1 = as1.equals(bs1)
    //val r2 = bs1.equals(cs1)
    as1.equals(bs1) && bs1.equals(cs1)
  }
}
