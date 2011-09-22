package projecteuler

object Project20 {

  def main(args : Array[String]) {

    var v : scala.BigInt = 1

    for (i <- 1 to 100) {

      v = v * i
    }

    val s = v.toString

    println("value = " + s)

    val xs = s.map(_.toString.toInt).toList

    println("xs = " + xs)

    var r = xs.foldLeft(0L)(_ + _)

    println("r = " + r)
  }
}
