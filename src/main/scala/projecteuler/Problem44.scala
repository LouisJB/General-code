package projecteuler

import annotation.tailrec

object Problem44 {

  def from(n : Int) : Stream[Int] = n #:: from(n + 1)
  lazy val pentsStream = from(1).map(pent(_))
  def pent(n : Int) : Int = (3 * (n*n) - n) / 2

  val pents = pentsStream.take(10000).toList

  def main(args : Array[String]) {

    import utils.Timer._

    time(r => println("Took " + r + "ms to run")) {

      val ans = find(pents, pents.drop(1))

      println("Answer = " + ans)

      /*
      //val ans = pents.find(p1 => pents.find(p2 => checkPent(p1, p2)) match {case Some(x) => true; case _ => false })

      val pentsZip = pents.zipWithIndex

      for (x1 <- pentsZip) {
        for (x2 <- pents.drop(x1._2 + 1)) {
          println(x1)
          if (checkPent(x1._1, x2)) {
            println("p1 = " + x1._1 + ", p2 = " + x2)
            return
          }
        }
      }
      */
    }
  }

  @tailrec
  def find(ls1 : List[Int], ls2 : List[Int]) : Int = {

    if (ls1.isEmpty) return 0

    def find2(ls2 : List[Int]) : Int = {

      if (ls2.isEmpty) return 0

      val x1 = ls1.head
      val x2 = ls2.head
      if (checkPent(x1, x2)) {
        println("p1 = " + x1 + ", p2 = " + x2 + ", |diff(p1 - p2)| = " + (x2 - x1))
        return (x2 - x1)
      }
      else {
        find2(ls2.tail)
      }
    }

    val r = find2(ls2)
    if (r == 0) {
      find(ls1.tail, ls2)
    }
    else {
      r
    }
  }

  def checkPent(x : Int, y : Int) : Boolean =
    isPent(x + y) && isPent(y - x)

  //http://en.wikipedia.org/wiki/Pentagonal_number
  def isPent(x : Int) = {
    val n = (math.sqrt((24 * x) + 1) + 1 ) / 6
    n == n.toInt
  }
}
