package tests.chessproblem2.npieces

import Pieces._
import utils.Timer._

object NPieces {

  val DEFAULT_SHOW_SOLUTIONS = true

  def main(args : Array[String]) {

    val start = System.currentTimeMillis()
    println("Starting...")
    println("(run with --help for usage)")

    if (args.length == 1 && args(0) == "--help") printHelp

    else if (args.length >= 1) {

      //findMoves(piece, digit, phoneNoLength, showNumbers)
    }
    else {
      Tests.doTests()

      val pieceSpec0 : List[(Piece, Int)] = List((Queen, 8))
      val ps0 = ProblemSpec(8, 8, makePieces(pieceSpec0))
      println(ps0.pieces)
      findMoves(ps0)

      val pieceSpec1 : List[(Piece, Int)] = List((King, 2), (Rook, 1))
      val ps1 = ProblemSpec(3, 3, makePieces(pieceSpec1))
      println(ps1.pieces)
      findMoves(ps1)

      val pieceSpec2 : List[(Piece, Int)] = List((Rook, 2), (Knight, 4))
      //val ps2 = ProblemSpec(4, 4, makePieces(pieceSpec2))
      //println(ps2.pieces)
      //findMoves(ps2)

      // actual problem...
      val pieceSpec3 : List[(Piece, Int)] = List(
          (Rook, 2),
          (Knight, 4),
          (Bishop, 4),
          (King, 4),
          (Queen, 4))
      //val ps3 = ProblemSpec(7, 7, makePieces(pieceSpec3))
      //println(ps3.pieces)
      //findMoves(ps3)
    }

    println("Completed in " + (System.currentTimeMillis() - start) +  "ms");
  }

  def findMoves(ps : ProblemSpec) : List[List[PiecePos]] = {

    val result = time(r => println("Took %d".format(r))) {
      val result = solveAll(ps)

      if (DEFAULT_SHOW_SOLUTIONS) printBoards(result, ps.n, ps.m)

      println("result size: %d".format(result.size))
      result.foreach(l => println(l.mkString(", ")))

      result
    }

    result
  }

  def makePieces(ls : List[(Piece, Int)]) : List[Piece] =
    for (pt <- ls; pn <- 1 to pt._2)
      yield pt._1

  def solveAll(ps : ProblemSpec) : List[List[PiecePos]] = {

    def placePiece(pl : List[Piece], cpiece : Piece) : List[List[PiecePos]] = {

      println("placePiece for " + pl)
      pl match {
        case Nil => List(List())
        case piece :: ppl => {
          for {
            row <- (1 to ps.m).toList
            column <- 1 to ps.n
            pieces <- placePiece(ppl, piece) // getPiece(ppl, piece), piece)
            if !pieces.exists(p => p.pos.x == row && p.pos.y == column)
            newPiece = PiecePos(piece, Pos(row, column))
            if isSafe(newPiece, pieces)
          } yield newPiece :: pieces
        }
      }
    }

    val r = placePiece(ps.pieces, null)

    println("unfiltered size = " + r.size)

    val r2 = r.filter(x => x.size == ps.pieces.size)

    val x = r2.map(s => s.sortWith((a, b) => a.pos.x < b.pos.x || a.pos.x == b.pos.x && a.pos.y <= b.pos.y))

    x.distinct
  }

  def solveAll2(ps : ProblemSpec) : List[List[PiecePos]] = {

    val pieceMap = collection.mutable.Map[Pos, Boolean]()
    def findPiece(p : Pos) : Boolean =
      if (pieceMap.contains(p)) true else { pieceMap(p) = true; false }

    def getPiece(pl : List[Piece], p : Piece) : List[Piece] =
      if (pl.size <= 1) pl else {
        val s = pl.span(e => e == p)
        s._2 ::: s._1
      }

    def nextPos(pos : Pos) : Pos = if (pos.x < ps.m) Pos(pos.x + 1, pos.y) else Pos(1, pos.y + 1)

    def placePiece(
        pl : List[Piece],
        currentSln : List[PiecePos],
        currentPiece : Piece,
        currentPos : Pos,
        currentSlns : List[List[PiecePos]]) : List[List[PiecePos]] = {

      println("placePiece for " + pl + ", " + currentSln)
      pl match {
        case Nil => if (currentSln.size == ps.pieces.size) currentSln :: currentSlns else currentSlns
        case piece :: ppl => {
          //val pos = nextPos(currentPos)
          if (currentPos.y <= ps.n) {
            if (!currentSln.exists(p => p.pos.x == currentPos.x && p.pos.y == currentPos.y)) {
              val newPiece = PiecePos(piece, currentPos)
              if (isSafe(newPiece, currentSln))
                placePiece(ppl, newPiece :: currentSln, piece, nextPos(currentPos), currentSlns) ::: placePiece(pl, currentSln, piece, nextPos(currentPos), currentSlns)  ::: placePiece(ppl, currentSln, piece, currentPos, currentSlns)
              else
                placePiece(ppl, currentSln, piece, currentPos, currentSlns) ::: placePiece(pl, currentSln, piece, nextPos(currentPos), currentSlns)
            }
            else
              placePiece(pl, currentSln, piece, nextPos(currentPos), currentSlns)
          }
          else
            placePiece(ppl, currentSln, piece, Pos(1, 1), currentSlns)
        }
      }
    }

    val r = placePiece(ps.pieces, Nil, null, Pos(1, 1), List[List[PiecePos]]())

    println("unfiltered size = " + r.size)

    val r2 = r.filter(x => x.size == ps.pieces.size)

    val x = r2.map(s => s.sortWith((a, b) => a.pos.x < b.pos.x || a.pos.x == b.pos.x && a.pos.y <= b.pos.y))

    x.distinct
  }

  def isSafe(piece : PiecePos, pieces : List[PiecePos]) =
    !pieces.exists(p => p.piece.inCheck(p.pos, piece.pos) || piece.piece.inCheck(piece.pos, p.pos))

  def printHelp {
    println("Usage: NPieces <piece>")
  }

  def printBoards(ls : List[List[PiecePos]], n : Int, m : Int) {
    def printBoard(ls : List[PiecePos]) {
      for (y <- 1 to m) {
        for (x <- 1 to n) {
          ls.find(p => p.pos.x == x && p.pos.y == y) match {
            case Some(p) => print(p.piece.charValue)
            case _ => print("-")
          }
        }
        println("")
      }
      println("")
    }

    for (b <- ls) printBoard(b)
  }

  def solveNQueens(n : Int, m : Int, nq : Int) : List[List[PiecePos]] = {
    def placePiece(k : Int) : List[List[PiecePos]] =
      if (k == 0)
        List(Nil)
      else
        for {
          pieces <- placePiece(k - 1)
          column <- 1 to n
          piece = PiecePos(Queen, Pos(k, column))
          if isSafe(piece, pieces)
        } yield piece :: pieces

    placePiece(nq)
  }

  case class ProblemSpec(n : Int, m : Int, pieces : List[Piece]) {

    def apply(s : String) : List[(Piece, Int)] = {
      val ls = s.split(" ").toList

      val k = ls.filter(c => c == "K").size
      val ks = if (k > 0) List((King, k)) else Nil

      val q = ls.filter(c => c == "Q").size
      val qs = if (q > 0) List((Queen, q)) else Nil

      ks ::: qs
    }
  }
}

object Pieces {

  case class PiecePos(piece : Piece, pos : Pos) {
    override def toString() = piece.charValue + " @ " + pos
  }

  sealed trait Piece {
    val Id : String
    def inCheck(p1 : Pos, p2 : Pos) = true
    def charValue : String = Id
    override def toString() = charValue

    def apply(s : String) = s match {
      case Queen.Id => Queen
      case King.Id => King
      case Rook.Id => Rook
      case Bishop.Id => Bishop
      case Knight.Id => Knight
    }
  }

  case object Queen extends Piece with XYMoves with DiagonalMoves {
    val Id = "Q"
    override def inCheck(p1 : Pos, p2 : Pos) = inCheckXy(p1, p2) || inCheckDiag(p1, p2)
  }

  case object King extends Piece with XYMoves with DiagonalMoves {
    val Id = "K"
    override def inCheck(p1 : Pos, p2 : Pos) = inCheckXy(p1, p2, 1) || inCheckDiag(p1, p2, 1)
  }

  case object Bishop extends Piece with DiagonalMoves {
    val Id = "B"
    override def inCheck(p1 : Pos, p2 : Pos) = inCheckDiag(p1, p2)
  }

  case object Rook extends Piece with XYMoves {
    val Id = "R"
    override def inCheck(p1 : Pos, p2 : Pos) = inCheckXy(p1, p2)
  }

  case object Knight extends Piece with LMoves {
    val Id = "N"
    override def inCheck(p1 : Pos, p2 : Pos) = inCheckL(p1, p2)
  }

  trait XYMoves {
    def inCheckXy(p1 : Pos, p2 : Pos) : Boolean = inCheckXy(p1, p2, Int.MaxValue)

    def inCheckXy(p1 : Pos, p2 : Pos, n : Int) =
      (p1.x == p2.x && (p1.y - p2.y).abs <= n) ||
      (p1.y == p2.y && (p1.x - p2.x).abs <= n)
  }

  trait DiagonalMoves {
    def inCheckDiag(p1 : Pos, p2 : Pos) : Boolean = inCheckDiag(p1, p2, Int.MaxValue)

    def inCheckDiag(p1 : Pos, p2 : Pos, n : Int) =
      (p1.x - p2.x).abs == (p1.y - p2.y).abs && // on diagonal
      (p1.x - p2.x).abs <= n
  }

  trait LMoves {
    def inCheckL(p1 : Pos, p2 : Pos) =
      ((p1.x - p2.x).abs == 1 && (p1.y - p2.y).abs == 2) ||
      ((p1.x - p2.x).abs == 2 && (p1.y - p2.y).abs == 1)
  }
}

// simple x-y position class
case class Pos(x : Int, y : Int) {

  def isRow(row : Int) = y == row
  def isCol(col : Int) = x == col
  def +(x : Int, y : Int) = Pos(this.x + x, this.y + y)
  def +(pos : Pos) = Pos(this.x + pos.x, this.y + pos.y)

  override def toString() = "{ " + x + ", " + y + " }"
}


// ***** TESTS: some very simple base test cases
object Tests {

  def doTests() {

    println("running self tests")

    // pawn start at "0" len 3 = 3 moves
    //assert(PhoneChess.findMoves(Pawn(), Key(0), 3, false) == 7)

    println("self-tests passed")
  }
}
