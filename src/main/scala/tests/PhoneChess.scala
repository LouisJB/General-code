package tests

import PhonePad._
import Pieces._

object PhoneChess {
  
  val DEFAULT_PHONE_NUMBER_LEN = 10
  val DEFAULT_SHOW_NUMBERS = false
  val DEFAULT_START_KEY = Key(5)
  val DEFAULT_START_PIECE = Queen

  def main(args : Array[String]) {

    val start = System.currentTimeMillis()
    println("Starting...")
    println("(run with --help for usage)")
    
    if (args.length == 1 && args(0) == "--help") printHelp
    else if (args.length >= 1) {
      val piece = args(0)
      val digit = args(1).toInt
      val phoneNoLength = if (args.length >= 3) args(2).toInt else DEFAULT_PHONE_NUMBER_LEN
      val showNumbers = if (args.length >= 4) args(3).toBoolean else DEFAULT_SHOW_NUMBERS

      findMoves(piece, digit, phoneNoLength, showNumbers)
    }
    else {
      Tests.doTests()
      findMoves(DEFAULT_START_PIECE, DEFAULT_START_KEY)
    }

    println("Completed in " + (System.currentTimeMillis() - start) +  "ms");
  }

  def findMoves(pieceStr : String, digit : Int, phoneNoLength : Int, showNumbers : Boolean) : Int =  
    findMoves(Chess.piece(pieceStr), PhonePad.fromDigit(digit.toChar), phoneNoLength, showNumbers)

  def findMoves(piece : Piece, pos : Pos) : Int = findMoves(piece, pos, DEFAULT_PHONE_NUMBER_LEN, DEFAULT_SHOW_NUMBERS)

  def findMoves(piece : Piece, pos : Pos, phoneNoLength : Int, showNumbers : Boolean) : Int = {

    println("Finding moves for piece: " + piece + ", at position: " + pos + " (key: '" + PhonePad.keys(pos) + "')")
    var length = 0
    
    def findMovesR(piece : Piece, pos : Pos, move : List[Pos] /*, moves : List[String] */) {

      if (move.length == phoneNoLength) {
        if (showNumbers) {
          println(toDigitString(move))
        }
        length = length + 1
      }
      else {
        val nextMoves = getMoves(piece, pos)

        for (m <- nextMoves) {
          val nextPiece = if ((piece.isInstanceOf[Pawn]) && (m.isRow(0))) Queen else piece

          findMovesR(nextPiece, m, m :: move /*, moves */)
        }
      }
    }

    findMovesR(piece, pos, pos :: Nil /*, Nil */)
    
    println("Total number of moves = " + length)

    clearCache
    length
  }

  def toDigitString(moves : List[Pos]) : String = moves.reverse.map(PhonePad.keys(_)).mkString
  
  def printHelp {
    println("Usage: PhoneChess <piece> <start digit> [<length> <show moves>]")
    println("where: Piece : [Pawn|King|Queen|Rook|Kight|Bishop] start digit : [0-9] length : [1-10] showm moves : [true|false]")
  }
}

object Chess {
  // map string piece name to piece class
  private val pieces : Map[String, Piece] = Map(
    "Pawn" -> Pawn(),
    "Queen" -> Queen,
    "King" -> King,
    "Rook" -> Rook,
    "Bishop" -> Bishop,
    "Knight" -> Knight
  )
  
  def piece(s : String) = pieces(s)
}

object PhonePad {

  val MAX_ROWS = 4 // this is the max count, indexes are 0 based
  val MAX_COLS = 3
  val MAX_EXTENT = math.max(MAX_COLS, MAX_ROWS)
  
  val keys = Map(
    Pos(0, 0) -> "1",
    Pos(1, 0) -> "2",
    Pos(2, 0) -> "3",
    Pos(0, 1) -> "4",
    Pos(1, 1) -> "5",
    Pos(2, 1) -> "6",
    Pos(0, 2) -> "7",
    Pos(1, 2) -> "8",
    Pos(2, 2) -> "9",
    Pos(1, 3) -> "0"
  )

  // calculate position from digit, 0 is special case
  def fromDigit(s : Char) = {
    val p = s.toInt
    if (p == 0) {
      Pos(1, MAX_ROWS-1)
    }
    else {
      val x = (p-1) % MAX_COLS // note: phone digits are 1 based but rows/cols are zero based...
      val y = (p-1) / MAX_COLS
      Pos(x, y)
    }
  }

  def validMove(pos : Pos) : Boolean = validMove(pos.x, pos.y)
  
  def validMove(x : Int, y : Int) = {
    (((y != MAX_ROWS - 1) || ((x != 0) && (x != 2))) // check invalid lower corners
     && ((x >= 0) && (x < MAX_COLS) && (y >= 0) && (y < MAX_ROWS))) // check basic bounds
  }
}

object Pieces {
  val moveMap = scala.collection.mutable.Map[(Piece, Pos), List[Pos]]()

  def getMoves(piece : Piece, pos : Pos) : List[Pos] = {
    if (!moveMap.contains((piece, pos))) {
      val m = piece.getMoves(pos)
      moveMap.put((piece, pos), m)
      m
    }
    else moveMap((piece, pos))
  }

  def clearCache = moveMap.clear
  
  trait Piece {
    def getMoves(currentPos : Pos) : List[Pos]
  }

  case object King extends Piece with XYMoves with DiagonalMoves {
    def getMoves(currentPos : Pos) : List[Pos] = {
      (xyMoves(currentPos, 1) ::: diagonalMoves(currentPos, 1)).distinct
    }
  }

  case object Queen extends Piece with XYMoves with DiagonalMoves {
    def getMoves(currentPos : Pos) : List[Pos] = {
      (xyMoves(currentPos, MAX_EXTENT) ::: diagonalMoves(currentPos, MAX_EXTENT)).distinct
    }
  }

  case object Bishop extends Piece with DiagonalMoves {
    def getMoves(currentPos : Pos) : List[Pos] = {
      diagonalMoves(currentPos, MAX_EXTENT).distinct
    }
  }

  case object Knight extends Piece with LMoves {
    def getMoves(currentPos : Pos) : List[Pos] = {
      lMoves(currentPos).distinct
    }
  }

  case object Rook extends Piece with XYMoves {
    def getMoves(currentPos : Pos) : List[Pos] = {
      xyMoves(currentPos, MAX_EXTENT).distinct
    }
  }

  case class Pawn() extends Piece with UpMoves {
    var firstMove = true
    def getMoves(currentPos : Pos) : List[Pos] = {
      val amount = if (firstMove && (currentPos.y == 2 || currentPos.y == 3)) 2 else 1
      firstMove = false
      upMoves(currentPos, amount).distinct
    }
  }

  trait UpMoves {
    def upMoves(currentPos : Pos, n : Int) =
      (currentPos.y to currentPos.y - n by -1)
        .filter(y => validMove(currentPos.x, y))
        .map(y => Pos(currentPos.x, y)).toList
  }

  trait XYMoves {
    def xyMoves(currentPos : Pos, n : Int) =
      (currentPos.x - n to currentPos.x + n)
        .filter(x => validMove(x, currentPos.y))
        .map(x => Pos(x, currentPos.y)).toList :::
      (currentPos.y - n to currentPos.y + n)
        .filter(y => validMove(currentPos.x, y))
        .map(y => Pos(currentPos.x, y)).toList
  }

  trait DiagonalMoves {
    def diagonalMoves(currentPos : Pos, n : Int) =
      (-n to n)
        .filter(z => validMove(currentPos.x + z, currentPos.y + z))
        .map(z => Pos(currentPos.x + z, currentPos.y + z)).toList :::
      (-n to n)
        .filter(z => validMove(currentPos.x + z, currentPos.y - z))
        .map(z => Pos(currentPos.x + z, currentPos.y - z)).toList
  }

  trait LMoves {
    def lMoves(currentPos : Pos) = {
      // 8 L-shapped moves (2 ((2,1)(1,2) in each quadrant)
      var moves : List[Pos] = Nil

      for (xFlip <- -1 to 1 by 2) {   // x symmetry +/- 1
        for (yFlip <- -1 to 1 by 2) { // y symmetry +/- 1
          val pos1 = currentPos + Pos((2 * xFlip), (1 * yFlip))
          if (validMove(pos1)) moves = pos1 :: moves
          val pos2 = currentPos + Pos((1 * xFlip), (2 * yFlip))
          if (validMove(pos2)) moves = pos2 :: moves
        }
      }
      currentPos :: moves
    }
  }
}

// simple x-y position class
case class Pos(x : Int, y : Int) {

  def isRow(row : Int) = y == row
  def isCol(col : Int) = x == col
  def +(x : Int, y : Int) = new Pos(this.x + x, this.y + y)
  def +(pos : Pos) = new Pos(this.x + pos.x, this.y + pos.y)

  override def toString() = "{x = " + x + ", y = " + y + "}"
}

// Key conversion from digit character
object Key {
  def apply(digit : Char) = PhonePad.fromDigit(digit.toChar)
}

// ***** TESTS: some very simple base test cases
object Tests {  
  def doTests() {

    println("running self tests")

    // pawn start at "0" len 3 = 3 moves
    assert(PhoneChess.findMoves(Pawn(), Key(0), 3, false) == 7)

    // pawn start at "0" len 3 = 3 moves
    assert(PhoneChess.findMoves(Pawn(), Key(8), 2, false) == 3)

    // pawn start at "0" len 3 = 3 moves
    assert(PhoneChess.findMoves(Pawn(), Key(5), 2, false) == 2)

    // pawn start at "0" len 4 = 21 moves - tests conversion from pawn to queen
    assert(PhoneChess.findMoves(Pawn(), Key(0), 4, false) == 7 + 6 + 8 /* 8 from conversion to queen */)

    // knight start at "5" len 1 = 1 move
    assert(PhoneChess.findMoves(Knight, Key(5), 1, false) == 1)

    // knight start at "7" len 2 = 3 moves
    assert(PhoneChess.findMoves(Knight, Key(7), 2, false) == 3)

    // knight start at "7" len 3 = 10 moves
    assert(PhoneChess.findMoves(Knight, Key(7), 3, false) == 10)
    
    // queen start at "5" len 2 = 10 moves
    assert(PhoneChess.findMoves(Queen, Key(5), 2, false) == 10)

    // king start at "0" len 3 = 21 moves
    assert(PhoneChess.findMoves(King, Key(0), 3, false) == 21)

    // rook start at "5" len 2 = 6 moves
    assert(PhoneChess.findMoves(Rook, Key(5), 2, false) == 6)

    // rook start at "0" len 3 = 22 moves
    assert(PhoneChess.findMoves(Rook, Key(0), 3, false) == 22)

    // bishop start at "5" len 2 = 5 moves
    assert(PhoneChess.findMoves(Bishop, Key(5), 2, false) == 5)
    
    // bishop start at "0" len 2 = 6 moves
    assert(PhoneChess.findMoves(Bishop, Key(0), 2, false) == 3)

    println("self-tests passed")
  }
}
