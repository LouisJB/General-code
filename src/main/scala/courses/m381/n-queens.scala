package courses.m381

/*
#!/bin/sh
exec scala "$0" "$@"
!#
*/

// http://www.cse.unl.edu/~goddard/Courses/CSCE310J/Lectures/Lecture8-DynamicProgramming.pdf
object Nqueens {

  def queens(n : Int): List[List[(Int, Int)]] = {

    def placeQueens(k : Int): List[List[(Int, Int)]] =
      if (k == 0) 
        List(List())
      else 
        for {
          queens <- placeQueens(k - 1)
          column <- 1 to n
          queen = (k, column)
          if isSafe(queen, queens) 
        } yield queen :: queens
    
    placeQueens(n)
  }

  def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) = 
    queens forall (q => !inCheck(queen, q))

  def inCheck(q1: (Int, Int), q2: (Int, Int)) = 
    q1._1 == q2._1 ||  // same row
    q1._2 == q2._2 ||  // same column
    (q1._1 - q2._1).abs == (q1._2 - q2._2).abs // on diagonal

  def main(args : Array[String]) {

    val r = queens(8)

    println("size = " + r.size)
    r.foreach(println)
  }
}

//Nqueens.main(args)
