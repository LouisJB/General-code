package projecteuler

 import scala.annotation._
 
// 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
object Problem31 {

  def main(args : Array[String]) {

    val amount = 200
    val coinTypes = 1 :: 2 :: 5 :: 10 :: 20 :: 50 :: 100 :: 200 :: Nil

    println(findCoins(amount, Nil, coinTypes).length)
    //println(findCoins(amount, Nil, coinTypes).mkString(", "))

    //@tailrec
    def findCoins(amount : Int, cs : List[Int], denoms : List[Int]) : List[List[Int]] = {

      if (amount <= 0) cs :: Nil
      else {
        denoms match {
          case Nil => Nil
          case l :: xs =>
            if (l > amount) Nil
            else
              findCoins(amount - l, l :: cs, denoms) ::: findCoins(amount, cs, xs)
        }
      }
    }

    def findCoins2(
      amount : Int,
      cs : List[Int],
      denoms : List[Int]) : List[List[Int]] = {

      findCoinsTr(amount, Nil, List(0) :: Nil, coinTypes)
    }

    //@tailrec
    def findCoinsTr(
      amount : Int,
      cs : List[Int],
      acc : List[List[Int]],
      denoms : List[Int]) : List[List[Int]] = {

      if (amount == 0) cs :: acc
      else {
        denoms match {
          case Nil => Nil
          case l :: xs =>
            if (l > amount) Nil
            else
              acc.head :: findCoinsTr(amount - l, l :: cs, acc, denoms)
              acc.head :: findCoinsTr(amount, cs, acc, xs)
        }
      }
    }
  }
}
