package courses.m381

/*
#!/bin/sh
exec scala "$0" "$@"
!#
*/

import math._

// http://www.cs.cmu.edu/~avrim/451/lectures/lect1001.pdf
object LCS {

  // compute longest subsequences of two lists
  def lcs[T](ls1 : List[T], ls2 : List[T])  : List[T] = {

    val n1 = ls1.size
    val n2 = ls2.size

    val lcs = Array.ofDim[Int](n1 + 1,  n2 + 1)

    for (i <- (1 to n1)) {
      for (j <- (1 to n2)) {
        if (ls1(i-1) == ls2(j-1)) {
          lcs(i)(j) = lcs(i-1)(j - 1) + 1 // new longest subsequence
        }
        else {
          // else take the maximum including either previous max ls1(i) or ls2(j)
          lcs(i)(j) = max(lcs(i-1)(j), lcs(i)(j-1))
        }
      }
    }

    println("Solution value: " + lcs(n1)(n2)  + ", backtracking...")
   
    def backtrack(i : Int, j : Int) : List[T] = {
      
      if (i == 0 || j == 0) Nil
      else {
        if (lcs(i)(j) == lcs(i-1)(j)) {
          backtrack(i-1, j)
        }
        else if (lcs(i)(j) == lcs(i)(j-1)) {
          backtrack(i, j-1)
        }
        else {
          ls1(i-1) :: backtrack(i-1, j-1)
        }
      }
    }

    // iterative soln
    var i = n1
    var j = n2
    while (i > 0 && j > 0) {
      if (lcs(i)(j) == lcs(i-1)(j)) {
        i = i - 1
      }
      else if (lcs(i)(j) == lcs(i)(j-1)) {
        j = j - 1
      }
      else {
        i = i - 1
        j = j - 1
        println("char: " + ls1(i))
      }
    }

    backtrack(n1, n2).reverse
  }

  def main(args : Array[String]) {

    println("lcs(\"abazdc\", \"bacbad\") = " + lcs("abazdc".toList, "bacbad".toList))
    println("lcs(\"louisbotterill\", \"lindseyjupp\") = " + lcs("louisbotterill".toList, "lindseyjupp".toList))
  }
}

//LCS.main(args)

