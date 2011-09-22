package projecteuler

object Problem36 {

  def main(args : Array[String]) {

    //println(isPalindrome(585, 10))
    //println(isPalindrome(585, 2))

    val p = findPalindromes(1000000)

    val sum = Utils.sum(p)

    println("Problem36 = " + p.mkString(", "))
    println("Problem36 sum ans = " + sum)
  }

  def findPalindromes(n : Int) : List[Long] =
    for (i <- List.range(1, n) if (isPalindrome(i, 10) && isPalindrome(i, 2))) yield i.toLong

  def isPalindrome(n : BigInt, radix : Int) = {

    Utils.isPalindrome(n, radix)

    /*
    val s = n.toString(radix)
    val f = s.toList
    val r = s.toList.reverse

    //println("testing " + f.mkString + " and " + r.mkString)
    f.equals(r)
    */
  }
}
