package projecteuler

import math._


object Problem67 {

  def read : String => List[String] = fileName =>
    (scala.io.Source fromFile fileName getLines) toList

  def matrix : List[String] => List[List[Int]] = xs =>
    xs map (_ split " " map (_.trim.toInt) toList)

  def meld : (List[Int], List[Int]) => List[Int] = (xs, ys) =>
    (xs zip (ys zip ys.tail).map(x => x._1 max x._2)).map(x => x._1 + x._2)

  def meld2(bl: List[Int], sl : List[Int]) : List[Int] = ((bl, sl) : @unchecked) match {
    case (bf :: bs :: brest, sf :: srest) =>
      sf + max(bf, bs) :: meld2(bs :: brest, srest)
    case (bf :: brest, s) if (brest.size == 1 && s.size == 1) =>
      List(s.head + max(bf, brest.head))
    case (b, Nil) =>
      Nil
  }

  def solve : String => Int = fileName => (read andThen matrix)(fileName) reduceRight meld head

  def main(args : Array[String]) {

    val res = solve("/Users/chillipower_uk/dev/projects/testprojects/training/Training-Scala/src/main/resources/triangle67.txt")

    println("Project Euler 67 answer = " + res)
  }
}
