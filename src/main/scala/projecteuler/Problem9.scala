package projecteuler

object Problem9 {

  def main (args : Array[String]) {

    for (i <- 100 to 1000) {
      //println(i)
      for (j <- i to 1000) {
        for (k <- j to 1000 if (i + j + k <= 1000)) {
          //println(i + ", " + j + ", " + k)
          //println(i*i + ", " + j*j + ", " + k*k)
          if (i + j + k == 1000) {
            if ((i*i) + (j*j) == (k*k)) {

              println("i = " + i + ", j = " + j + ", k = " + k + ", " + (i*j*k))
            }
          }
        }
      }
    }
  }
}
