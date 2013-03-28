package projecteuler

import courses.m381.Partitions

object Problem78 {
  import Partitions._
  def main(args : Array[String]) {
    println("finding x in p(x) | mod 1M ...")
    val pMod1M = findPart(1000000)
    println("part mod 1M: " + pMod1M)
    assert(pMod1M == Some(55374))
  }
}


/* old junk
object Problem78 {

  def from(n : Int) : Stream[Int] = n #:: -n  #:: from(n + 1)
  lazy val pentsStream = from(1).map(pent(_))
  
  
  val parts = Array.ofDim[Int](1000)

  def pent(n : BigInt) : BigInt = (BigInt(3) * (n*n) - n) / BigInt(2)

  //val pents = Iterator.from(0).flatMap(n => Iterator(-n, n+1)).map(n => if (n == 0) BigInt(1) else { val n = BigInt(n); ((BigInt(3) * (n * n) - n) / BigInt(2))} )
  val pents = Iterator.from(0).flatMap(n => Iterator(-n, n+1)).map(x => if (x == 0) 1 else { val n = x; ((3 * (n * n) - n) / 2)} )

  def main(args : Array[String]) {

    println("res" + part(10))
  }

  def part(n : Int) : Int = {

    //if (parts(n) > 0) return parts(n)

    if (n <= 1) 1
    
    var p = 0
    val ks = pents.take(n).toList // .toIndexedSeq
    var k = ks.head
    while (k < n) {
      println(n + ", " + k)
      p += part(n - k)
      k = ks.head
    }
    p
  }

/*
  val map = scala.collection.mutable.Map[(Short, Short), Int]()

  def part(min : Short, n : Short) : Int = {
    if (min > n) 0
    else if (min == n) 1
    else {
      val n = map.getOrElseUpdate(((min + 1).toShort, n), part((min + 1).toShort, n))
      val y = map.getOrElseUpdate((min, (n - min).toShort), part(min, (n - min).toShort))
      n % 1000000 + y % 1000000
    }
  }
    */

/*
  def main(args : Array[String]) {

    //val res = Iterator.from(10).map(n => (n, partR(n, 100))).find(n => { println(n); n._2 % 100 == 0 })
    val res = Iterator.from(10).map(n => (n, partR(n+1, 1000000))).find(n => { println(n); n._2 % 1000000 == 0 })
    println("result is " + res)
  }

  val aiPn = Array.ofDim[Int](1000)

  aiPn(1) = 1
  def partR(n : Int, mod : Int) : Int = {

    if (n < 0) return 0

    // Use cached value if already calculated
    if (aiPn(n) > 0) return aiPn(n)

    var Pn = 0

    for (k <- 1 to n) {
      val n1 = (n - k * (3 * k - 1) / 2)  % mod
      val n2 = (n - k * (3 * k + 1) / 2)       % mod

      val Pn1 = partR(n1, mod)
      val Pn2 = partR(n2, mod)

      // elements are alternately added and subtracted
      if (k % 2 == 1)
        Pn = (Pn + Pn1 + Pn2)
      else
        Pn = (Pn - Pn1 - Pn2)
    }

    // Cache calculated valued
    //println("n = " + n + ", " + Pn)
    aiPn(n) = Pn % mod
    Pn
  }
*/
  /*
  val signs = Stream.continually(Seq(1, 1, -1, -1)).flatten
  //val pents = Stream.from(0).flatMap(n =>  -n :: n+1 :: Nil).zipWithIndex.map(n => (generalisedPentagonalNumber(n._1), n._2+1)).drop(1)

  val pents = Iterator.from(0).flatMap(n => Iterator(-n, n+1)).map(n => if (n == 0) 1 else { val n = BigInt(n); ((BigInt(3) * (n * n) - n) / BigInt(2))} )

  def main(args : Array[String]) {
    
    //pents.take(10).foreach(println)

    //def signedPents(n : Int) = pents.take(n).reverse.zip(signs).map(n => n._1._1 * n._2)

    //def sum(ls : Stream[BigInt]) = ls.foldRight(BigInt(0))((n, s) => n + s)

    val pentSeq = pents.take(500).toIndexedSeq

    println(pentSeq.mkString(", "))

    //Stream.from(1).map(n => (n, sum(signedPents(n-1)))).take(10).foreach(println)

    //def find(z : Int) =
    //    Stream.from(1).map(n => (n, sum(signedPents(n-1)))).find(y => { println(y); y._2 % z  == 0 })

    //println(find(1000))

    //def find(z : Int) =
    //  Stream.from(0).flatMap(n =>  -n :: n+1 :: Nil).zipWithIndex.map(n => (generalisedPentagonalNumber(n._1), n._2+1)).drop(1).find(y => (y._1 % z  == 0))

  }
  */
} */