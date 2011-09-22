package dynamicprogramming

// http://www.cs.ucdavis.edu/~amenta/w04/dis4.pdf
object SubsetSum {

  // Subset sun O(nB) running time (NP-Complete
  // a simpler boolean variation of the knapsack problem
  def subsetsum(ls : List[Int], n : Int) : List[Int] = {

    // use boolean 2d array, true = subset sum match for (sub)problem
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

    // recursively backtrack to recover the subset
    def backtrack(i : Int, j : Int) : List[Int] = {
      if (i > 0 && j > 0) {
        if (!sum(i-1)(j)) {
          println("** use item ", ls(i-1))
          ls(i-1) :: backtrack(i-1, j - ls(i-1))
        }
        else {
          backtrack(i-1, j)
        }
      }
      else Nil
    }

    backtrack(ls.size, n)
  }

  def main(args : Array[String]) {

    println("subsetsum(1, 4, 5, 2, 6)  = 9 = " + subsetsum(List(1, 4, 5, 2, 6), 9))
    println("subsetsum(1, 4, 5, 2, 6)  = 7 = " + subsetsum(List(1, 4, 5, 2, 6), 7))
    println("subsetsum(1, 4, 5, 2, 6)  = 8 = " + subsetsum(List(1, 4, 5, 2, 6), 8))
  }
}
