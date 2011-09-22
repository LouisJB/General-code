package projecteuler

import Utils._

object Problem43 {

  val fact8 = factorial(8).toInt
  val fact8_2 = (2 to 8).reduceLeft(_*_) // factorial by multiplying all digits 1 through n
  
  def main (args : Array[String]) {

    println("fact test = " + fact8 + ", " + fact8_2 + ", "
            +  fact8.equals(8*7*6*5*4*3*2*1))
    
    // test permute stream
    permute((0 to 9).toList).take(100).foreach(println)

    val test = 1406357289L
    println("test1 = " + Utils.isPandigital0_9(test))
    println("test2 = " + hasSubStringDivisibility(test))
    
    val start = 1023456789

    val rs = findSpecialPandigitals(start)

    val r = Utils.sum(rs)

    println("Sum = " + r)
  }

  def findSpecialPandigitals(n : Long) = {

    //var i = n
    var rs = List[Long]()
    //val end = 9876543210L
    
    //for (i <- n.toLong to BigInt("9876543210") if (Utils.isPandigital0_9(n))) {
    //while (i <= end) {

    // loop through the first two digits
    for (a <- 1 to 9) {
      for (b <- 0 to 9 if b != a) {
        println("a = " + a)
        println("b = " + b)

        // create list of remaing digits out of 0 through 9
        val digits = (0 to 9).toList.filter(z => !z.equals(a) && !z.equals(b))

        // permute the remaining digits
        val xs = permute(digits)

        // take the 8! permutations of the remianing 8 digits
        xs.take(fact8).foreach(ls => {
          val i = (a :: b :: ls).mkString.toLong
          
          if (hasSubStringDivisibility(i)) {

            rs = i :: rs
            println(rs.mkString(", "))
          }
        })
      }
    }

    rs
  }

  def hasSubStringDivisibility(n : Long) : Boolean = {

    for (i <- 2 to 8) {
      val r = hasSubStringDivisibility(n, i)
      if (!r) return r
    }
    
    true
  }

  def hasSubStringDivisibility(n : Long, i : Int) = {

    val s = n.toString
    val r = s.substring(i - 1, i + 2)

    val x = r.toInt
    val d = getDivisor(i)
    (x % d == 0)
  }

  def getDivisor(i : Int) : Int =
    if (i == 2) 2
    else if (i == 3) 3
    else if (i == 4) 5
    else if (i == 5) 7
    else if (i == 6) 11
    else if (i == 7) 13
    else 17
}
