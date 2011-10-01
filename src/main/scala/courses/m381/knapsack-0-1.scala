package courses.m381

/*
#!/bin/sh
exec scala "$0" "$@"
!#
*/

// http://www.cse.unl.edu/~goddard/Courses/CSCE310J/Lectures/Lecture8-DynamicProgramming.pdf
object Knapsack_0_1 {

  // list of (weight, value) tuples and mw, the max weight
  def knapsack_0_1(ls : List[(Int, Int)], mw : Int) : List[(Int, Int)] = {

    val n = ls.size
    val sum = Array.ofDim[Int](n + 1,  mw + 1)

    (1 to n).foreach(i => sum(i)(0) = 0)
    (0 to mw).foreach(w => sum(0)(w) = 0)

    for (i <- (1 to n)) {
      for (w <- (0 to mw)) {
        if (ls(i-1)._1 <= w) {
          if (ls(i-1)._2 + sum(i-1)(w-ls(i-1)._1) > sum(i-1)(w)) {
            sum(i)(w) = ls(i-1)._2 + sum(i-1)(w-ls(i-1)._1)
          }
          else {
            sum(i)(w) = sum(i-1)(w)
          }
        }
        else {
          sum(i)(w) = sum(i-1)(w)
        }
      }
    }

    println("Solution value: " + sum(n)(mw)  + ", backtracking...")
   
    var i = n
    var w = mw
    while (i > 0 && w > 0) {
      if (sum(i)(w) != sum(i-1)(w)) {
        println("Take item ", i, ls(i-1)._1 + ", value " + ls(i-1)._2)
        i = i - 1
        w = w - ls(i)._1
      }
      else {
        i = i - 1
      }
    }

    ls   
  }

  def main(args : Array[String]) {

    println("knapsack_0_1((2, 3), (3, 4), (4, 5), (5, 8), (9, 10), 20) = " + knapsack_0_1(List((2, 3), (3, 4), (4, 5), (5, 8), (9, 10)), 20))
    println("knapsack_0_1((2, 3), (3, 4), (4, 5), (5, 6), 5) = " + knapsack_0_1(List((2, 3), (3, 4), (4, 5), (5, 6)), 5))
  }
}

//Knapsack_0_1.main(args)

