package projecteuler

//The 5-digit number, 16807=7^(5), is also a fifth power. Similarly, the 9-digit number, 134217728=8^(9), is a ninth power.
object Problem63 {

  def main(args : Array[String]) {

    val r1 = find(8)
    val r2 = find(7)

    var count = 0
    for (a <- Stream.from(1)) {

      count = count + find(a)
      if (a % 1000 == 0) println("a = " + a + ", count = " + count)
    }
  }

  def find(a : Int) : Int = {

    var count = 0
    var iter = 0
    for (n <- Stream.from(1)) {

      val x = BigInt(a).pow(n)
      val l = x.toString.length
      if (l > n) return count
      if (l == n) {

        println("" + a + "^" + n + " = " + x)
        count = count + 1
      }
      iter = iter + 1
      if (iter > 50) return count
    }
    count
  }
}
