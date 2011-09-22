package projecteuler

object Problem53 {

  def main(args : Array[String]) {

    val test1 = Utils.combinations(5, 3)
    println("5C3 = " + test1)

    val test2 = Utils.combinations(23, 10)
    println("23C10 = " + test2)

    var count = 0
    for (n <- 1 to 100) {
      for (k <- 1 to n) {
        val x = Utils.combinations(n, k)
        if (x > 1000000L) {
          count = count + 1
        }
      }
    }
    println("Ans: " + count)
  }
}
