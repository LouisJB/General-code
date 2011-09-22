package projecteuler

// Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
object Problem23 {

  def main(args : Array[String]) {

    println(getDivisorsSum(12))
    println(isAbundant(12))

    val max = 28123

    var ls = List[Long]()

    for (i <- 1L to max) {

      if (!isAbundantSum(i)) {

        ls = i :: ls
        //println(ls)
      }
    }
    println(ls)
    println(sum(ls))
  }

  def isAbundantSum(n : Long) : Boolean = {

    for (i <- 2L to n - 1) {

      if ((isAbundant(i)) &&
          (isAbundant(n-i))) return true
    }
    false
  }

  def isAbundant(n : Long) = { getDivisorsSum(n) > n }

  def sum(ls : List[Long]) = ls.foldLeft(0L)(_+_)

  def getDivisorsSum(n : Long) = sum(getDivisors(n))

  def getDivisors(n : Long) = {

    var list : List[Long] = Nil

    for (i <- 1 to math.sqrt(n).toInt) {
      if (n % i == 0) {
        list = i :: list

        if ((i != 1) && (i != math.sqrt(n))) {

          list = (n / i) :: list
        }
      }
    }
    //println(list)
    list
  }
}
