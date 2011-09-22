package projecteuler

object Problem145 {

  def main(args : Array[String]) {

    def reversible(n : Int) : Boolean = n % 10 != 0 && isAllOdd(n + reverse(n))

    def reverse(n : Int) = n.toString.reverse.toInt

    def isAllOdd(n : Int) = !asDigitList(n).exists(x => x % 2 == 0)

    def asDigitList(n : Int) : List[Int] = n.toString.toList.map(x => x.asDigit)

    //val r = (1 to 1000).filter(n => reversible(n)).size

    val r = (1 to 1000000000).filter(n => reversible(n)).size

    println("Problem145 = " + r)
  }
}