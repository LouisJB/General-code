package problems

/**
 * Created by IntelliJ IDEA.
 * User: chillipower_uk
 * Date: 09/01/2011
 * Time: 13:58
 * To change this template use File | Settings | File Templates.
 */

import RichInt._

//Sort a list of numbers in which each number is at a distance k from its actual position
object SortKthDistance {

  def swap(ar : Array[Int], a : Int, b : Int) : Array[Int] = { val t = ar(a); ar(a) = ar(b); ar(b) = t; ar  }

  def swapk(ar : Array[Int], k : Int) : Array[Int] = {

    val n = ar.size

    if ((k != 1) && (!(n isPower k) || !(n isEven))) throw new Exception("n not even or k does not divide array n * n times (k^n) != n n = " + n)

    for (y <- (0 until n/k) by 2) {
      for (x <- (0 until k)) {

        println("n = " + x + ", y = " + y)
        swap(ar, (y * k) + x, (y * k) + x + k)
      }
    }
    ar
  }

  def swapk[T](ar : Array[T], k : Int) =
    ar.grouped(k).sliding(1).grouped(2).toList.flatMap(x => List(x(1), x(0))).flatten.flatten

  def main(args : Array[String]) {

    val arrs = List((16, 4), (64, 8), (8, 1))
    val swapResults = arrs.map(e => ((1 to e._1).toArray, e._2)).map(e => (swapk(e._1, e._2)))

    swapResults.foreach(e => println(e.mkString(", ")))
  }
}

case class RichInt(n : Int) {

  def isPow2 = isPower(2)
  def isPower(b : Int) = (logb(b) _ andThen isInt)(n)
  def isEven = n % 2 == 0
}

object RichInt {

  implicit def toRichInt(n : Int) : RichInt = RichInt(n)

  def logb(b : Int)(n : Int) = math.log(n) / math.log(b)
  def isInt(x : Double) = x.toInt == x
  def isPow2(n : Int) = (logb(2) _ andThen isInt)(n)
}
