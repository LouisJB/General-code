package sudoku

import annotation.tailrec
import sudoku.Sudoku._
import RichList._

object RichList {
  implicit def toRichList[T](ls : List[T]) = RichList(ls)
}
case class RichList[T](ls : List[T]) {
  import util.Random.nextInt
  def permute = ls.map(x => (x, nextInt)).sortWith(_._2 > _._2).map(_._1)
}

object Sudoku {

  def N = NullCell

  trait Puzzle {
    def isSolved : Boolean
  }
  trait Board {
    def getRow(r : Int) : Vector[Cell]
    def getCol(c : Int) : Vector[Cell]
    def update(row : Int,  col : Int, square : Cell) : Board
    def solve : (Puzzle, Int)
  }
  
  case object UnsolvablePuzzle extends Puzzle {
    def isSolved = false
  }
  
  //@tailrec
  final def solveP(p : Puzzle, n : Int = 0) : Puzzle = {
    //println("solving: \n" + p.toString)
    if (p.isSolved) p
    else {
      p match {
        case s : SudokuPuzzle => {
          val x = fillInPossibilities(s)
          x match {
            case s : SudokuPuzzle => {
              solveP(s, n+1) match {
                case z : SudokuPuzzle => solveP(z)
                case _ => solveP(s.permute)
              }
            }
            case _ => UnsolvablePuzzle
          }    
        }
        case _ => p
      }
    }
  }

  def fillInPossibilities(b : SudokuPuzzle) : Puzzle = {
    if (b.isSolved) return b
    val changes : List[(Int,  Int,  Cell)] =
      (0 to b.size - 1).flatMap(r => {
        val row = b.getRow(r)
        (0 to b.size - 1).flatMap(c => {
          val col = b.getCol(c)
          val currentSqr = row(c)
          currentSqr match {
            case NullCell | ChoiceCell(_) => {
              val m1 = missingDigits(row)
              val m2 = missingDigits(col)
              val m3 = missingDigits(r, c, b.squares)
              val possibleDigits = m1.intersect(m2).intersect(m3)
              currentSqr match { 
                case ChoiceCell(n) if (possibleDigits == n) => None
                case _ => {
                  if (possibleDigits.size > 1)
                    Some(r,  c,  ChoiceCell(possibleDigits))
                  else if (possibleDigits.size == 1)
                    Some(r, c, FinalCell(possibleDigits.head))
                  else return UnsolvablePuzzle
                }
              }
            }
            case _ : FinalCell => None
          }
        })
      }).toList

    if (changes.isEmpty) // && !b.isSolved)
      b.permute
    else
      changes.foldRight(b)((c, t) => t.update(c._1, c._2, c._3))
  }

  def checkDigits(v : Vector[Cell]) = {
    val xs = v.flatMap(e => e.value).groupBy(identity)
    xs.keySet.forall(k => xs(k).size == 1) &&
      (1 to v.size).forall(k => xs.contains(k))
  }

  def missingDigits(v : Vector[Cell]) : List[Int] = {
    val s = v.flatMap(_.value).toSet
    (1 to v.size).filterNot(x => s.contains(x)).toList
  }

  // assumption, using 3x3 unique grids when size > 3
  def missingDigits(r : Int, c : Int, v : Vector[Vector[Cell]]) : List[Int] = {
    if (v.size > 3) {
      def getPos(x : Int) = ((x / 3) * 3)
      val sr = getPos(r)
      val sc = getPos(c)
      val ls =
        for (x <- (sr to sr+2); y <- (sc to sc+2))
          yield v(x)(y)

      val s = Set() ++ ls.flatMap(_.value)
      (1 to v.size).filterNot(x => s.contains(x)).toList
    }
    else (1 to v.size).toList
  }
}

case class SudokuPuzzle(name : String = "", squares : Vector[Vector[Cell]]) extends Board with Puzzle {
  import Sudoku._
  val size = squares.size
  def getRow(r : Int) : Vector[Cell] = squares(r)
  def getCol(c : Int) : Vector[Cell] = squares.map(r => r(c))

  def update(row : Int,  col : Int, square : Cell) : SudokuPuzzle = {
    val newRow = squares(row).updated(col, square)
    SudokuPuzzle(this.name, squares.updated(row, newRow))
  }

  def isSolved =
    (0 to size - 1).forall(r => checkDigits(getRow(r))) &&
      (0 to size - 1).forall(c => checkDigits(getCol(c)))
  
  def solve : (Puzzle, Int) =
    (Stream.continually(solveP(this)).find(_.isSolved).get, 0)
    
  def permute : Puzzle = {
    val choices =
      (0 to size-1).flatMap(r => squares(r).zipWithIndex
        .map(x => (r, x._2, squares(r)(x._2))))
        .collect{ case (r, c, s @ ChoiceCell(z)) => (r, c, s) }
        .toList

    if (choices.isEmpty && !this.isSolved)
      return UnsolvablePuzzle

    val s = choices.permute.head
    this.update(s._1, s._2, FinalCell(s._3.permute.numbers.head))
  }

  override def toString = squares.map(_.mkString(" ")).mkString("\n")
}
object Cell {
  implicit def toSquare(n : Int) = FinalCell(n)
}
trait Cell {
  def value : Option[Int]
}
case object NullCell extends Cell {
  def value : Option[Int] = None
  override def toString = "N"
}
case class ChoiceCell(numbers : List[Int]) extends Cell {
  def value : Option[Int] = None
  def permute : ChoiceCell = {
    this.copy(numbers = numbers.permute)
  }
  def sortedChoices = numbers.sortWith(_ > _)
  override def toString = "<" + numbers.mkString(", ") + ">"
  override def equals(that : Any) = that match {
    case other @ ChoiceCell(n) if (this.sortedChoices == other.sortedChoices) => true
    case _ => false
  }
}
case class FinalCell(n : Int) extends Cell {
  def value : Option[Int] = Some(n)
  override def toString = n.toString
}

object SudokuTest extends App {
  import Sudoku._
  
  def solve(puzzle : SudokuPuzzle) {
    val s = puzzle.solve
    println("p " + puzzle.name + " solves in %d as\n%s".format(s._2, s._1))
  }

  solve(SudokuPuzzle("p1",
    Vector(
      Vector(1, 2, 3),
      Vector(2, 3, 1),
      Vector(3, 1, 2)
    )
  ))

  solve(SudokuPuzzle("p2",
    Vector(
      Vector(1, 2, N),
      Vector(2, 3, 1),
      Vector(3, 1, 2)
    )
  ))

  solve(SudokuPuzzle("p3",
    Vector(
      Vector(1, 2, N),
      Vector(2, N, 1),
      Vector(N, 1, 2)
    )
  ))

  solve(SudokuPuzzle("p4",
    Vector(
      Vector(N, 2, N),
      Vector(2, N, 1),
      Vector(N, 1, 2)
    )
  ))

  solve(SudokuPuzzle("p5",
      Vector(
        Vector(N, 2, N),
        Vector(2, N, 1),
        Vector(N, 1, N)
      )
    ))

  // http://www.sudokucollection.com/sp/Sudokux.html
  solve(SudokuPuzzle("p6-ve1",
    Vector(
      Vector(N, N, 9, 7, 3, N, 5, 2, 6),
      Vector(N, N, 5, N, 2, N, 8, N, N),
      Vector(6, N, 8, N, N, N, N, 4, 7),
      Vector(N, N, N, N, N, 9, N, 6, 2),
      Vector(N, 4, N, 6, N, 3, N, 8, N),
      Vector(8, 9, N, 5, N, N, N, N, N),
      Vector(2, 6, N, N, N, N, 1, N, 8),
      Vector(N, N, 7, N, 1, N, 6, N, N),
      Vector(9, 5, 1, N, 6, 4, 2, N, N)
    )
  ))

  solve(SudokuPuzzle("p7-ve100",
    Vector(
      Vector(N, 5, 4, 7, 1, N, N, N, 6),
      Vector(2, N, 8, 6, 4, 5, N, 9, 1),
      Vector(N, N, N, 8, N, N, N, 4, N),
      Vector(N, 1, N, N, 2, 6, N, 5, 9),
      Vector(4, N, 6, N, 5, N, 3, N, 8),
      Vector(5, 2, N, 4, 8, N, N, 1, N),
      Vector(N, 4, N, N, N, 1, N, N, N),
      Vector(6, 8, N, 2, 7, 4, 1, N, 5),
      Vector(1, N, N, N, 3, 8, 9, 6, N)
    )
  ))

  solve(SudokuPuzzle("p7-e001",
    Vector(
      Vector(N, N, 6, 2, 4, N, N, N, N),
      Vector(2, 7, N, N, N, N, N, N, N),
      Vector(8, 4, N, N, 7, 5, N, 3, N),
      Vector(N, 8, N, N, N, 1, N, N, N),
      Vector(N, 1, N, 4, N, 9, N, 5, N),
      Vector(N, N, N, 3, N, N, N, 1, N),
      Vector(N, 3, N, 1, 9, N, N, 4, 8),
      Vector(N, N, N, N, N, N, N, 6, 1),
      Vector(N, N, N, N, 5, 8, 7, N, N)
    )
  ))

  solve(SudokuPuzzle("p8-m001",
    Vector(
      Vector(7, N, 4, N, 1, N, N, N, N),
      Vector(N, 2, N, N, N, N, N, N, N),
      Vector(9, 1, 6, N, N, 7, N, N, 2),
      Vector(8, N, 9, 1, N, N, N, N, N),
      Vector(2, N, N, 5, N, 6, N, N, 7),
      Vector(N, N, N, N, N, 9, 8, N, 5),
      Vector(4, N, N, 2, N, N, 7, 1, 6),
      Vector(N, N, N, N, N, N, N, 2, N),
      Vector(N, N, N, N, 6, N, 5, N, 4)
    )
  ))

  solve(SudokuPuzzle("p9-h001",
    Vector(
      Vector(1, N, N, N, N, 4, N, 3, N),
      Vector(N, N, N, 3, N, N, 9, 8, 4),
      Vector(9, N, N, 6, N, N, N, 7, N),
      Vector(N, N, N, N, N, 9, 7, 5, N),
      Vector(N, N, 6, N, N, N, 8, N, N),
      Vector(N, 1, 7, 2, N, N, N, N, N),
      Vector(N, 5, N, N, N, 6, N, N, 2),
      Vector(6, 7, 9, N, N, 3, N, N, N),
      Vector(N, 4, N, 5, N, N, N, N, 7)
    )
  ))

  solve(SudokuPuzzle("p10-h100",
    Vector(
      Vector(2, N, N, 5, 4, N, N, 3, N),
      Vector(N, N, 8, N, N, 9, N, 5, N),
      Vector(N, N, N, N, N, N, N, N, 1),
      Vector(5, N, N, N, 9, 2, N, N, N),
      Vector(1, N, 9, 6, N, 8, 3, N, 4),
      Vector(N, N, N, 1, 3, N, N, N, 7),
      Vector(7, N, N, N, N, N, N, N, N),
      Vector(N, 8, N, 2, N, N, 6, N, N),
      Vector(N, 2, N, N, 8, 5, N, N, 9)
    )
  ))

  solve(SudokuPuzzle("p11-h002",
    Vector(
      Vector(1, N, 7, N, 2, N, 3, N, 8),
      Vector(5, N, N, N, N, 9, N, N, 6),
      Vector(N, N, N, 1, N, N, 5, N, N),
      Vector(N, N, N, N, 4, N, N, 2, 3),
      Vector(N, N, N, N, N, N, N, N, N),
      Vector(9, 6, N, N, 3, N, N, N, N),
      Vector(N, N, 9, N, N, 4, N, N, N),
      Vector(2, N, N, 7, N, N, N, N, 1),
      Vector(3, N, 4, N, 5, N, 6, N, 7)
    )
  ))
}
