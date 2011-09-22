package utils


object Division {

  // divide by successive subtraction (recursive)
  def divide(n : Int, d : Int) : Double = {

    if (d <= 0) throw new Exception("Div by zero error")

    var dd = 0.0

    def divide(n : Int, d : Int) : Double = {
      if (n < d) dd
      else {
        dd = dd + 1
        divide (n - d, d)
      }
    }

    divide(n, d)
  }

  // divide by successive subtraction (recursive)
  def divide2(n : Int, d : Int) : Double = {

    if (d <= 0) throw new Exception("Div by zero error")

    var dd = 0.0

    def divide(n : Int, d : Int, x : Double) : Double = {

      println("dd = " + dd)

      val res = dd * d

      if ((n - res) < d) dd
      else {
        dd = dd + 1
        divide (n, d, x - d)
      }
    }

    divide(n, d, n)
  }

  def divideI(n : Int, d : Int) : Double = {

    var nn = n;
    var i = 0;
    while (nn >= d) {
        nn = nn - d;
        i = i + 1;
    }
    i
  }

  // divide, successively shift divisor left then subtract
  def divideS(n : Int, d : Int) : Int = {

    var q = 1;
    var nn = n;
    if (d > n) 0
    else if (d == n) 1
    else {

      var dd = d

      // shift left (^2) until the divisor >= numerator
      while (dd << 1 <= n) {
        dd = dd << 1
        q = q << 1
      }

      // subtract d from n until d >= n
      while (dd <= nn - d) {
        nn = nn - d
        q = q + 1
      }
      q   // q is the integer quotient
    }
  }

  def main(args: Array[String]) = {

    println(">>Division")

    val y = divideS(80, 3)

    val vals = (1, 1) :: (80, 3) :: (84, 3) :: (3, 1) :: (5, 5) :: (0, 6) :: (5, 0) :: Nil

    vals.foreach((e) => doTest(e._1, e._2))

    def doTest(n : Int, d : Int) {

      try {
        val x = divide(n, d)
        printRes(n, d, x)
        val y = divideI(n, d)
        printRes(n, d, y)
        val z = divideS(n, d)
        printRes(n, d, z)
        assert (x == y && y == z)
      } catch {
        case e : Exception =>
          println("divide " + n + "/" + d + " raised an exception: "
            + e.getMessage())
      }
    }

    def printRes(n : Int, d : Int, x : Double) {

      println("dividing " + n + " by " + d + " = " + x + " mod = " + n % x
        + " - check: (x+r/d)*d = " + (x + (n % x)/d) * d)
    }

    println("<<Division")
  }
}
