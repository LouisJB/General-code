package projecteuler

object Problem29 {

  def main(args : Array[String]) {

    println("Sum = " + getPowers(100, 100))
  }

  def getPowers(n : Int, m : Int) = {

    var ls : List[BigInt] = Nil

    for (a <- 2 to n) {
      for (b <- 2 to m) {

        val x = BigInt(a).pow(b)
        ls = x :: ls
      }
    }
    println(ls.mkString(", "))
    
    ls.distinct.size
  }
  
  def toList(n : BigInt) : List[BigInt] = n.toString.map(x => BigInt(x.toString)).toList

  def sum(ls : List[BigInt]) = ls.foldLeft(BigInt(0L))(_+_)
}

