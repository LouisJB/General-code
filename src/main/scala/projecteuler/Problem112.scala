package projecteuler

object Problem112 {

  def isBouncyList(ls : List[Int]) : Boolean = { val asc = ls.sortWith(_ < _); asc != ls && asc.reverse != ls }
  def isBouncy(n : Int) : Boolean = isBouncyList(n.toString.toList.map(x => x.asDigit))

  def main(args : Array[String]) {

    var count = 0
    var m = 0
    val r = (1 to 10000000).find(n => {

      if (isBouncy(n)) {

        count = count + 1

        val p = count.toDouble / n.toDouble

        if (n % 1000 == 0) println("n = " + n + ", " + count + ", p = " + p)
        p >= .99
      }
      else false
    })

    println("Problem112 = " + r)
  }
}