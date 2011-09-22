package projecteuler

object Problem38 {

  def main(args : Array[String]) {

    var max = BigInt(0)
    for (n <- 1 to 10000) {
     val y = getPandigitalProduct(n)

     if (y > max) {
       max = y
       println("n = " + n + ", max = " + max)

     }
    }
  }

  def getPandigitalProduct(n : Int) : BigInt = {

    var s = n.toString()
    //println("n = " + n)
    for (m <- 2 to 10) {

      val y = BigInt(n) * m

      s = s + y.toString();

      if (s.length > 9) return 0
      
      val z = BigInt(s)
      //println(s)
      if (Utils.isPandigital1_9(z)) return z
    }

    0
  }
}
