package projecteuler

// difference between sum of squares and square of sums
object Problem6 {
	
  def main (args : Array[String]) {

    // n = 10
    // 3025 - 385 = 2640
    val r1 = 1 to 10
    val result1 = getDifference(r1.toList)
    println("squareSums - sumSquares n=10 : " + result1)

    val r2 = 1 to 100
    val result2 = getDifference(r2.toList)
    println("squareSums - sumSquares n=100 : " + result2)
  }

  def getDifference(ls : List[Int]) = {

    println("range : " + ls.mkString)
    val sumSquares = ls.map(x => x*x).foldLeft(0)(_ + _)
    println("sumSquares = " + sumSquares)

    var squareSums = ls.foldLeft(0)(_ + _)
    squareSums = squareSums * squareSums;
    println("squareSums = " + squareSums)

    squareSums - sumSquares;
  }
}
