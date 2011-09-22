package projecteuler

object Problem21 {

  def main (args : Array[String]) {

    val test0 = getDivisors(16)
    println(test0)

    val test1 = getDivisors(220)
    println(test1)
    val testsum1 = test1.foldLeft(0L)(_+_)
    println(testsum1)

    val test2 = getDivisors(testsum1)
    println(test2)
    val testsum2 = test2.foldLeft(0L)(_+_)
    println(testsum2)

    val ls = getAmmicablePairs(10000)

    println("Ammicable pairs " + ls)
    val sum = ls.foldLeft(0L)(_+_)

    println("Sum = " + sum)
  }

  def getAmmicablePairs(m : Int) = {

    var list : List[Long] = Nil

    for (n <- 1 to m) {
      //println("n = " + n)
      val sum = getDivisorsSum(n)

      if (sum != n) {
        val sum2 = getDivisorsSum(sum)

        if (n == sum2) {
          if (n < m) {
            if (!list.contains(n)) {
              println("adding1 " + n)
              list = n :: list
            }
          }
          if (sum < m) {
            if (!list.contains(sum)) {
              println("adding2 " + sum)
              list = sum :: list
            }
          }
        }
      }
    }

    list
  }

  def getDivisorsSum(n : Long) = getDivisors(n).foldLeft(0L)(_+_)

  def getDivisors(n : Long) = {

    var list : List[Long] = Nil

    for (i <- 1 to math.sqrt(n).toInt) {
      if (n % i == 0) {
        list = i :: list

        if ((i != 1) && (i != math.sqrt(n).toInt)) {

          list = (n / i) :: list
        }
      }
    }

    list
  }
}
