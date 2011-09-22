package utils

import scala.util.Random;

object Probabilities {

  val rand = new Random(System.currentTimeMillis());

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]) = {
    println("starting")

    var b = 0
    var g = 0
    var n = 0   // number fo children
    var mfs = 0  // max family size (number of children in family)
    var x = false

    while (n < 1000000) {
      var fs = 0
      do {
        x = f();

        if (x == true) {
          print("b")
          b = b + 1
        } else {
          print("g")
          g = g + 1
        }
        fs = fs + 1
        n = n + 1
      }
      while (x == false)

      if (fs > mfs) mfs = fs
      println();
    }

    println("\rDone: " + mfs);
    println("b = " + b + " g = " + g 
      + " g/b = " + g.asInstanceOf[Double]/b.asInstanceOf[Double])
  }

  def f() : Boolean = {

    val x = rand.nextInt(2)

    if (x == 1) true else false
  }
}
