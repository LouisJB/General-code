package courses.m381

/*
#!/bin/sh
exec scala "$0" "$@"
!#
*/

// http://www.cse.unl.edu/~goddard/Courses/CSCE310J/Lectures/Lecture8-DynamicProgramming.pdf
object CoinChange {

  // list of coins and change, the required change
  def coinChange(ls : List[Int], change : Int) : List[Int] = {

    val n = change
    val C = Array.ofDim[Int](n + 1)
    val denoms = Array.ofDim[Int](n + 1)

    for (c <- (1 to change)) {
      C(c) = 10000
      for (i <- (1 to ls.size)) {
        if (ls(i - 1) <= c) {
          if ((1 + C(c - ls(i - 1))) < C(c)) {
            C(c) = 1 + C(c - ls(i - 1))   
            denoms(c) = ls(i - 1)
          }
        }
      }
    }

    println("Solution value: " + C(n)  + ", backtracking...")
   
    def printCoins(j : Int) {
      if (j > 0) {
        printCoins(j - denoms(j))
        println("Using coin of value " + denoms(j))
      }
    }

    printCoins(n)
/*
    var i = n
    while (i > 0) {
      println("n = " + n)
      if (C(i) != C(i-1)) {
        println("Using coin of value ", C(i-1))
        i = i - C(i-1)
      }
    }
*/
    ls 
  }

  def main(args : Array[String]) {

    println("coinChange((1, 10, 25), 30) = " + coinChange(List(1, 10, 25), 30))
    println("coinChange((1, 2, 5, 10, 20, 50), 145) = " + coinChange(List(1, 2, 5, 10, 20, 50, 100), 145))
    println("coinChange((1, 2, 5, 10, 20), 110)) = " + coinChange(List(1, 2, 5, 10, 20), 110))
    println("coinChange((1, 5, 100), 172)) = " + coinChange(List(1, 5, 100), 172))
  }
}

//CoinChange.main(args)

