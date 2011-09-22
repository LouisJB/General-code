package projecteuler;

object Problem92 {

  def main (args : Array[String]) {

    val x = (1 to 10000000).map(x => getDigitChain(x)).filter(x => x.size > 0 && x.last == 89).size

    println("Project Euler 92 = " + x)
  }

  def getDigitChain(n : Int) : List[Int] = {

    val x = digitSum(n)
    if (x == 89 || x == 1) return x :: Nil
    else
      x :: getDigitChain(x)
  }

  def digitSum(n : Int) : Int = {

    n.toString.map(_.asDigit).map(x => x * x).sum
  }
}
