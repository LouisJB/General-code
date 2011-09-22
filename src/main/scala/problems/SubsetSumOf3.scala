package problems

import collection.mutable.Map

object SubsetSumOf3 {

  def subSum(ls : List[Int], matchVal : Int = 0) : Boolean = {

    val n = ls.size
    val map = Map[Int, (Int, Int)]()

    // O(n^2)
    for (i : Int <- 0 until n) {
      for (j : Int <- 0 until n) {
        map(matchVal - (ls(i) + ls(j))) = (i, j)
      }
    }

    // O(n)
    for (z <- 0 until n) {
      map.get(ls(z)) match {
        case Some((i, j)) if (z != i && z != j) => return true
        case _ =>
      }
    }

    false
  }

  def main(args : Array[String]) {

    val problems =
      List(
        (List(1, 2, 3), 0),
        (List(4, 6, 9, 2, -5, -2, -9, -1, 7), 0),
        (List(4, 6, 9, 2, -5, -2, -9, -1, 7), 5),
        (List(4, 6, 9, 2, -5, -2, -9, -1, 7), 99))

    problems.foreach(p => {
      println("Subset sum of 3 for %s found: %b".format(p._1.mkString(", "), subSum(p._1, p._2)))
    })
  }
}
