#!/bin/sh
exec scala -deprecation "$0" "$@"
!#

object SubsetSum {

  def subsetsum(ls : List[Int], n : Int) : List[Int] = {
    val sum = Array.ofDim[Boolean](ls.size + 1,  n+1)

    (0 to ls.size).foreach(i => sum(i)(0) = true)
    (1 to n).foreach(j => sum(0)(j) = false)

    for (k <- (1 to ls.size)) {
      for (s <- (1 to n)) {
        sum(k)(s) = sum(k-1)(s) || ls(k-1) <= s && sum(k-1)(s - ls(k-1))
      }
    }

    println("Backtracking...")
   
    var j = n
    for (i <- (ls.size to 1 by -1)) {
      if (!sum(i-1)(j)) {
        println("use item ", ls(i-1))
        j = j - ls(i-1)
      }
    }
    
    ls   
  }

  def main(args : Array[String]) {

    println("subsetsum(1, 4, 5, 2, 6)  = 9 = " + subsetsum(List(1, 4, 5, 2, 6), 9))
    println("subsetsum(1, 4, 5, 2, 6)  = 7 = " + subsetsum(List(1, 4, 5, 2, 6), 7))
    println("subsetsum(1, 4, 5, 2, 6)  = 8 = " + subsetsum(List(1, 4, 5, 2, 6), 8))
  }
}

SubsetSum.main(args)

