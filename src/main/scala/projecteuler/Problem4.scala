package projecteuler

object Problem4 {

  def main (args : Array[String]) {

    var max = 0
    var i = 999
    var j = 999

    while (i > 0) {
      while (j > 0) {
        val x = (i * j).toString()
        val y = x.substring(0, x.length/2)
        val z = x.substring(x.length/2, x.length).reverse
        println("i = " + i + ", j = " + j + ", n = " + x + ", y = " + y + ", z = " + z)

        if (y == z) {
          if ((i*j) > max) {
            max = math.max(i*j, max)
            println("found - " + x + ", i = " + i + ", j=" + j)
          }
        }

        j = j - 1
      }
      j = 999
      i = i - 1
    }

    println(max)
  }
}
