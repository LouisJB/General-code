package sudoku

trait Board {
  def getRow : Vector[Int]
  def getCol : Vector[Int]
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
  
  def getRow : Vector[Int] = null
  def getCol : Vector[Int] = null
  
  def update(row : Int,  col : Int, square : Square) : Board = {
    val newRow = squares(row).updated(col, square)
    Puzzle(squares.updated(row, newRow))
  }

  def isSolved = {
    true
  }

  def solve = {
    if (isSolved) {
      this
    }
    else
      null
  }
  
  override def toString = squares.map(_.mkString(" ")).mkString("\n")
}

trait Square
case object NullSquare extends Square
case class ChoiceSquare(numbers : List[Int]) extends Square {
}
case class FinalSquare(n : Int) extends Square {

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
}
