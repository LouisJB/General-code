package projecteuler

object Problem39 {

  def main(args : Array[String]) {

    //println("120 = " + findCombs(120).map(_.toString).mkString(", "))

    findMax(1000)
  }

  def findMax(n : Int) {

    // run through each of the values, then sort desc
    //val r = (1 to n).map(n => (n, findCombs(n).length)).sortWith((a, b) => a._2 > b._2)

    val r = (1 to n).map(x => (x, findCombs(x).length))
      .reduceLeft((a, b) => if (a._2 > b._2) a else b)

    println("r = " + r.toString)

    val r2 = (1 to n).map(x => (x, findCombs(x).length))
    val max = Utils.maximize(r2)(a => a._2)
    println("max = " + max)
  }

  def findCombs(n : Int) : List[(Int, Int, Int)] = {

    var solutions = List[(Int, Int, Int)]()

    for (a <- 1 to n/4) {
      for (b <- a to n/2) {
        val c = n - (a + b)
        if (a + b + c == n) {
          if (a*a + b*b == c*c) {
            //println("a=" + a + ",b=" + b + ",c=" + c)
            solutions = (a, b, c) :: solutions
          }
        }
      }
    }

    //println("length = " + solutions.length)
    solutions
  }

/*
  def maximize[A, B <% Ordered[B]](ds: Iterable[A])(fn: A => B): A =
    ds.map(a => (a, fn(a)))
      .reduceLeft((p1,p2) => if(p2._2 > p1._2) p2 else p1)
      ._1
  def minimize[A, B <% Ordered[B]](ds: Iterable[A])(fn: A => B): A =
    ds.map(a => (a, fn(a)))
      .reduceLeft((p1, p2) => if(p2._2 < p1._2) p2 else p1)
      ._1
*/
}
