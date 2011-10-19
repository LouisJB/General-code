package sudoku

trait Board {
  def getRow(r : Int) : Vector[Square]
  def getCol(c : Int) : Vector[Square]
  def update(row : Int,  col : Int, square : Square) : Board
  def solve : Board
}
  
case object Puzzle {
  def apply(size : Int) : Puzzle = {
    var squares : Vector[Vector[Square]] = Vector.tabulate(size, size)((r : Int,  c : Int) => NullSquare)
    Puzzle(squares)      
  }
}
case class Puzzle(squares : Vector[Vector[Square]]) extends Board {
  val size = squares.size
  def getRow(r : Int) : Vector[Square] = squares(r)
  def getCol(c : Int) : Vector[Square] = squares.map(r => r(c))
  
  def update(row : Int,  col : Int, square : Square) : Puzzle = {
    val newRow = squares(row).updated(col, square)
    Puzzle(squares.updated(row, newRow))
  }

  def checkDigits(v : Vector[Square]) = {
    val xs = v.flatMap(e => e.value).groupBy(e => e)
    xs.keySet.forall(k => xs(k).size == 1) &&
    (0 to size - 1).forall(k => xs.contains(k+1))
  }
  
  def missingDigits(v : Vector[Square]) : List[Int] = {
    val s = Set() ++ v.flatMap(_.value)
    s.filterNot(x => (1 to size).exists(_ == x)).toList
    (1 to size).filterNot(x => s.contains(x)).toList
  }
  
  def isSolved = {
    (0 to size - 1).forall(r => checkDigits(getRow(r))) &&
      (0 to size - 1).forall(c => checkDigits(getCol(c)))
  }

  def solve = {
    if (isSolved) {
      this
    }
    else
      fillInPossibilities
  }
  
  def fillInPossibilities : Puzzle = {
    val changes : List[(Int,  Int,  Square)] =
      (0 to size - 1).flatMap(r => {
        val row = getRow(r)
        (0 to size - 1).flatMap(c => {
          val col = getCol(c)
          val currentSqr = row(c)
          currentSqr match {
            case NullSquare => {
              val m1 = missingDigits(row)
              val m2 = missingDigits(col)
              val possibleDigits = m1.intersect(m2).toList
              if (possibleDigits.size > 1)
                Some(r,  c,  ChoiceSquare(possibleDigits))
              else
                Some(r, c, FinalSquare(possibleDigits.head))
            }
            case _ : ChoiceSquare => None
            case _ : FinalSquare => None
          }
        })
      }).toList
    changes.foldRight(this)((c, t) => t.update(c._1, c._2, c._3))
  }
  
  override def toString = squares.map(_.mkString(" ")).mkString("\n")
}

trait Square {
  def value : Option[Int]
}
case object NullSquare extends Square {
  def value : Option[Int] = None
}
case class ChoiceSquare(numbers : List[Int]) extends Square {
  def value : Option[Int] = None
}
case class FinalSquare(n : Int) extends Square {

  def value : Option[Int] = Some(n)
  override def toString = n.toString
}

object Sudoku extends App {
  implicit def toSquare(n : Int) = FinalSquare(n)
  val p1 = new Puzzle(
    Vector(
      Vector(1, 2, 3),
      Vector(2, 3, 1),
      Vector(3, 1, 2)
    )
  )
  
  val s1 = p1.solve
   
  println("p1 solves as\n" + s1)

  val p2 = new Puzzle(
    Vector(
      Vector(1, 2, NullSquare),
      Vector(2, 3, 1),
      Vector(3, 1, 2)
    )
  )

  val s2 = p2.solve

  println("p2 solves as\n" + s2)
}
