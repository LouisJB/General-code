package projecteuler

import math._

trait PokerHand {
  val cards : List[Card]
  val rank : Int
  val value : Int
  val remainingCards : List[Card] = Nil

  protected def sortedCards = cards.sortWith(_.value.n > _.value.n)
  protected def sortedValues = cards.map(_.value.n).sortWith(_ > _)
  protected val values = cards.groupBy(_.value)
  protected def nOfAKind(n : Int) = values.filter(_._2.size == n).map(_._2).toList
  override def toString =
    "%s %d %s".format(this.getClass.getSimpleName, value, cards.mkString(","))
}

object PokerHand {
  def highestCard(ls : List[(Int, Int)]) = ls.takeWhile(x => x._1 >= x._2).exists(x => x._1 > x._2)
  def winningHand(lh : PokerHand, rh : PokerHand) =
    (lh.rank > rh.rank) ||
      ((lh.rank == rh.rank) && (lh.value > rh.value) ||
        ((lh.rank == rh.rank) && (lh.value == rh.value) &&
      highestCard(lh.remainingCards.map(_.value.n).zip(rh.remainingCards.map(_.value.n)))))
}
case class HighCard(cards : List[Card]) extends PokerHand { // Highest value card.
  val rank = 1
  override val remainingCards = sortedCards.tail
  override val value = sortedCards.head.value.n
}
case class OnePair(cards : List[Card]) extends PokerHand { // Two cards of the same value.
  val rank = 2
  val pair = nOfAKind(2).flatten
  override val remainingCards = sortedCards.filter(c => !pair.contains(c))
  override val value = pair.head.value.n
}
case class TwoPairs(cards : List[Card]) extends PokerHand { // Two different pairs.
  val rank = 3
  val pairs = nOfAKind(2).sortWith(_.head.value.n > _.head.value.n)
  override val remainingCards = sortedCards.filter(c => !pairs(0).contains(c) && !pairs(1).contains(c))
  override val value = max(pairs(0).head.value.n, pairs(1).head.value.n) * 100 +
    min(pairs(0).head.value.n, pairs(1).head.value.n)
}
case class ThreeOfAKind(cards : List[Card]) extends PokerHand { // Three cards of the same value.
  val rank = 4
  val triple = nOfAKind(3).flatten
  override val remainingCards = sortedCards.filter(c => !triple.contains(c))
  override val value = triple.head.value.n
}
case class Straight(cards : List[Card]) extends PokerHand { // All cards are consecutive values.
  val rank = 5
  override val value = sortedValues.head
}
case class Flush(cards : List[Card]) extends PokerHand { // All cards of the same suit.
  val rank = 6
  override val remainingCards = sortedCards.tail
  override val value = sortedValues.head
}
case class FullHouse(cards : List[Card]) extends PokerHand { //, pair : OnePair, triple : ThreeOfAKind) extends PokerHand { // Three of a kind and a pair.
  val rank = 7
  override val value = nOfAKind(3).flatten.head.value.n * 100 + nOfAKind(2).flatten.head.value.n
}
case class FourOfAKind(cards : List[Card]) extends PokerHand { // Four cards of the same value.
  val rank = 8
  override val remainingCards = nOfAKind(1).flatten
  override val value = nOfAKind(4).flatten.head.value.n
}
case class StraightFlush(cards : List[Card]) extends PokerHand { // All cards are consecutive values of same suit.
  val rank = 9
  override val value = sortedValues.max
}
case class RoyalFlush(cards : List[Card]) extends PokerHand { // Ten, Jack, Queen, King, Ace, in same suit.
  val rank = 10
  override val value = Int.MaxValue
}
case class Hand(cards : List[Card]) {
  import Value._
  def bestHand : PokerHand = {
    val suits = cards.groupBy(_.suit)
    val values = cards.groupBy(_.value)
    val sortedValues = cards.sortWith(_.value() > _.value())
    
    if (suits.size == 1) {
      val minValue = sortedValues.last.value()
      val maxValue = sortedValues.head.value()
      if ((minValue == TEN.n) && (maxValue == ACE.n))
        RoyalFlush(cards)
      else if ((maxValue - minValue) == 4)
        StraightFlush(cards)
      else
        Flush(cards)
    }
    else if (values.size == 2) {
      if (values.exists(_._2.size == 2))
        FullHouse(cards)
      else {
        FourOfAKind(cards)
      }
    }
    else if (values.size == 3) {
      if (values.exists(_._2.size == 3))
        ThreeOfAKind(cards)
      else
        TwoPairs(cards)
    }
    else if (values.size == 4)
      OnePair(cards)
    else {
      if ((sortedValues.head.value.n - sortedValues.last.value.n) == 4)
        Straight(cards)
      else
        HighCard(cards)
    }
  }

  override def toString = "[" + cards.mkString(" ") + "]"
}
case object Hand {
  def apply(s : String) : Hand  = {
    apply2(s.split(" ").toList)
  }
  def apply2(ls : List[String]) : Hand  = {
    Hand(ls.map(Card(_)))
  }
}

trait Suit {
  def symbol : String
  override def toString = symbol
}

case object Clubs extends Suit { val symbol = "C" }
case object Spades extends Suit { val symbol = "S" }
case object Diamonds extends Suit { val symbol = "D" }
case object Hearts extends Suit { val symbol = "H" }

object Suit {
  def apply(s : Char) = s match {
    case 'C' => Clubs
    case 'S' => Spades
    case 'D' => Diamonds
    case 'H' => Hearts
  }
}
trait ValueTrait
case class Value(n : Int) {
  import Value._
  def apply() = n
  override def toString = valueToSymMap(n).toString
}
object Value {
  val values = (1 to 9).map(x => x.toString()(0) -> x).toList ::: List('T' -> 10, 'J' -> 11, 'Q' -> 12, 'K' -> 13, 'A' -> 14)
  val symToValueMap : Map[Char, Int] = values.toMap
  val valueToSymMap : Map[Int, Char] = symToValueMap.map(_.swap)
  
  def apply(c : Char) : Value = Value(symToValueMap(c))
  case object TEN extends Value(10)
  case object ACE extends Value(14)
}

trait CardTrait
case class Card(value : Value, suit : Suit) {
  override def toString = "%s %s".format(value.toString, suit.toString)
}
case object Card {
  def apply(s : String) : Card =
    Card(Value(s(0)), Suit(s(1)))
}

object Problem54 {
  import PokerHand._
  def main(args : Array[String]) {
    val hands = List(
      "5H 5C 6S 7S KD",
      "1H 3C 5S 7S 9D",
      "1H 1D 1S 7S 9D",
      "1H 1D 1S 1C 9D",
      "1H 1D 2S 2H 9D",
      "1H 3H 4H 5H 6H",
      "1H 2H 3H 4H 5H",
      "TH JH QH KH AH",
      "TH JH QH KH AC"
    ).map(Hand(_))
    
    val bestHands = hands.map(h => "hand %s, best hand %s".format(h, h.bestHand))
    bestHands.foreach(println)
    println("\n-\n")
    
    val testHands = List(
      ("5H 5C 6S 7S KD 2C 3S 8S 8D TD", false),
      ("5D 8C 9S JS AC 2C 5C 7D 8S QH", true),
      ("2D 9C AS AH AC 3D 6D 7D TD QD", false),
      ("4D 6S 9H QH QC 3D 6D 7H QD QS", true),
      ("2H 2D 4C 4D 4S 3C 3D 3S 9S 9D", true),
      ("2H 2D 3C 3D AS 3C 3D 4S 4S 1D", false),
      ("2H 2D 2C 2D AS 9C 9D 9S 4S 4D", true),
      ("1H 2H 3H 4H 5H 2C 3D 4S 5S 6D", true),
      ("1S 2H 3H 4H 5H 2C 2D 2S 2S 3D", false),
      ("1S 2H 3H 4H 5H TD JD QD KD AD", false),
      ("KC 7H QC 6D 8H 6S 5S AH 7S 8C", false))
    
    val allTestHands = testHands.map(h => (h._1.split(" ").splitAt(5), h._2))
    val testWins = allTestHands.map(h =>
      (h._1._1.toList, h._1._2.toList,
        Hand.apply2(h._1._1.toList).bestHand,
        Hand.apply2(h._1._2.toList).bestHand,
        winningHand(Hand.apply2(h._1._1.toList).bestHand, Hand.apply2(h._1._2.toList).bestHand),
        h._2))
    
    val errors = testWins.filter(r => r._5 != r._6)
    println("errors: " + errors.mkString(", "))
    val testWinCount = testWins.filter(_._5 == true)
    //testWins.foreach(println)
    println("no of test wins = " + testWinCount.size + " out of " + testWins.size)

    // actual data
    val allHandsLines = scala.io.Source.fromFile("/Users/chillipower_uk/dev/projects/testprojects/training/Training-Scala/resources/euler/problem54-poker.txt")
    val allHands = allHandsLines.getLines().map(h => h.split(" ").splitAt(5))
    val wins = allHands.map(h =>
      (h._1.toList, h._2.toList, winningHand(Hand.apply2(h._1.toList).bestHand, Hand.apply2(h._2.toList).bestHand))).toList
    
    val winCount = wins.filter(_._3 == true).size
    //wins.foreach(println)
    println("no of wins = " + winCount + " out of " + wins.size)
    assert(winCount == 376)
  }
}
