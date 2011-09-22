package projecteuler

object Problem14 {

  def main (args :Array[String]) {

    //val values = Array.tabulate(10000000)(n => 0)
    val values = scala.collection.mutable.Map[Long, Int]()
    var maxCount = 0
    var maxVal = 0

    println("test for 13 = " + calc(13))

    def count(n : Int) = {

      for (i <- 1 to n) {

        val c = calc(i)
        values(i) = c
        //println("i = " + i + " c = " + c)
        if (c > maxCount) {
          println("maxCount = " + c + ", maxVal = " + i)
          maxCount = c
          maxVal = i
        }
      }
      maxVal
    }

    def calc(n : Long) = {
      var r = n
      var c = 1
      while (r > 1) {
        if (values.getOrElse(r, 0) > 0) {
          c = c + values(r)
          r = 1L
        }
        else {
          c = c + 1
          r = nextValue(r)
        }
      }
      c
    }

    def nextValue(n : Long) : Long = {

      if (n % 2 == 0) {
        n / 2
      }
      else {
        3 * n + 1
      }
    }

    val mv = count(1000000)
    println("Max value = " + mv + " count = " + values(mv))
  }
}
