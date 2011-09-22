package projecteuler

object Problem56 {

  def main(args : Array[String]) {

    val res = findMaxSum(100, 100)

    println("Problem 56 = " + res)
  }

  def findMaxSum(n : Int, m : Int) : Long = {

    var max = 0L
    for (a <- 0 until n) {

      for (b <- 0 until m) {

        val x = BigInt(a).pow(b)
        val s = x.toString.map(_.toString.toLong)
        val sum = Utils.sum(s.toList)

        if (sum > max) {
          max = sum
          println("max found " + sum + ", a = " + a + ", b = " + b)
          //println("str = " + s)
        }
      }
    }

    max
  }
}
