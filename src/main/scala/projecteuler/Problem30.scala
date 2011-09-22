package projecteuler

object Problem30 {

  def main (args : Array[String]) {

    val ls = findPowers(5)
    println("Powers = " + ls.mkString(","))
    println("Sum = " + sum(ls))
  }

  def findPowers(n : Int) = {

    var ls = List[BigInt]()
    for (i <- BigInt(2) to 1000000) {

      val xs = toList(i)

      val x = sum(xs.map(_.pow(n)))

      if (x == i) {

        ls = i :: ls
      }
    }

    ls
  }

  def toList(n : BigInt) : List[BigInt] = n.toString.map(x => BigInt(x.toString)).toList

  def sum(ls : List[BigInt]) = ls.foldLeft(BigInt(0L))(_+_)
}
