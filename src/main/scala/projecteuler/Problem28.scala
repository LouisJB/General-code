package projecteuler

// What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way
object Problem28 {

  def main(args : Array[String]) {

    println("Sum = " + getSum(1001))
  }
  
  def getSum(n : Int) = {
    var x = 1
    var p = 1
    for (d <- 3 to n by 2) {
      for (s <- 1 to 4) {
        p = p + (d-1)
        x = x + p
        //println("adding p to, p = " + p + " n = " + n)
      }
    }
    x
  }
}
