package dynamicprogramming

import scala.math._

// LCS function appears to work and construct the right table of results
// and longest common subsequence length but the backtracking only
// finds one solution and needs more work...
object lcs {

  def lcs(x : String, y : String) = {

    var c : Array[Array[Option[Int]]]
      = Array.tabulate(x.length+1)(_ => Array.tabulate(y.length+1)(_ => None))

    def lcsR(x : String, y : String,
             i : Int, j : Int, c : Array[Array[Option[Int]]]) : Int = {

      if (i >= 0 && j >= 0) {
        c(i)(j) match {
          case None =>
            if (x(i) == y(j)) {
              c(i)(j) = Some(lcsR(x, y, i-1, j-1, c) + 1)
            }
            else {
              c(i)(j) = Some(max(lcsR(x, y, i-1, j, c),
                                 lcsR(x, y, i, j-1, c)))
            }
          case _ =>
        }
        c(i)(j).getOrElse(0)
      }
      else {
        c(x.length)(y.length).getOrElse(0)
      }
    }
    val len = lcsR(x, y, x.length-1, y.length-1, c)

    // output the DP 2d-array
    println("c = " + c.foreach(x => {x.foreach(print); println}))

    val lcss1 = backtrack(x, y, c)
    println("lcss = " + lcss1.mkString(","))

    val lcss2 = backtrack2(x, y, c)
    println("lcss = " + lcss2.mkString(","))

    len
  }

  def backtrack(x : String, y: String,
                c : Array[Array[Option[Int]]]) : List[String] = {
  
    // backtrack to recover all lcss
    def backtrackR(i : Int, j : Int,
        lcss : List[String]) : List[String] = {

      if (i < 0 || j < 0) {
        "" :: Nil
      }
      else if (x(i) == y(j)) {
        backtrackR(i-1, j-1, lcss).map(x(i) + _)
      }
      else {
        (if (c(i)(j-1).getOrElse(0) >= c(i-1)(j).getOrElse(0))
          lcss ::: backtrackR(i, j-1, lcss)
        else Nil) :::
        (if (c(i-1)(j).getOrElse(0) >= c(i)(j-1).getOrElse(0))
          lcss ::: backtrackR(i-1, j, lcss)
        else Nil)
      }
    }
    backtrackR(x.length-1, y.length-1, Nil)
  }

  def backtrack2(x : String, y: String,
                c : Array[Array[Option[Int]]]) : List[String] = {

    // backtrack to recover all lcss
    def backtrackR(i : Int, j : Int,
        lcs : List[Char],
        lcss : List[String]) : List[String] = {

      if (i > 0 && j > 0) {
        if (x(i-1) == y(j-1)) {
          backtrackR(i-1, j-1, x(i-1) :: lcs, lcss)
        }
        else {
          (if (c(i-1)(j).getOrElse(0) >= c(i)(j-1).getOrElse(0))
            backtrackR(i-1, j, lcs, lcss)
          else Nil) :::
         (if (c(i)(j-1).getOrElse(0) >= c(i-1)(j).getOrElse(0))
              backtrackR(i, j-1, lcs, lcss)
            else
              Nil)
        }
      }
      else {
        //println("adding - " + lcs + " to " + lcss.mkString(","))
        lcs.mkString :: lcss
      }
    }
    backtrackR(x.length, y.length, Nil, Nil)
  }

  def main(args : Array[String]) {

    if (args.length < 2) {
      val s1 = "abcbdabse"
      val s2 = "bdcabaef"
      println(lcs(s1, s2))
    }
    else {
      println(lcs(args(0), args(1)))
    }
  }
}
