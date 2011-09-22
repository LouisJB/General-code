package projecteuler

object Problem55 {

  def main(args : Array[String]) {

    println("testLychrel(196, 50) = " + testLychrel(196, 50))
    println("testLychrel(47, 50) = " + testLychrel(47, 50))
    println("testLychrel(349, 50) = " + testLychrel(349, 50))
    println("testLychrel(46, 50) = " + testLychrel(46, 50))

    val ls = findLychrels(10000, 50)

    println("Problem 55 lychrels = " + ls.reverse.mkString(", "))
    println("Problem 55 Ans = " + ls.length)
  }

  def findLychrels(n : Int, m : Int) = {

    var ls = List[Long]()
    for (i <- 1 until n) {
      println("testing " + i)
      if (testLychrel(i, m))
        ls = i :: ls
    }

    ls
  }

  def testLychrel(n : Long, m : Int) : Boolean = {

    var x = BigInt(n)
    for (i <- 1 until m) {
      val rs = x.toString.toList.reverse
      val r = BigInt(rs.mkString)

      x = x + r

      if (isPalindrome(x)) return false
    }

    true
  }

  def isPalindrome(n : BigInt) = {

    val f = n.toString.toList
    val r = n.toString.toList.reverse

    //println("testing " + f.mkString + " and " + r.mkString)
    f.equals(r)
  }
}
