package projecteuler


//145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
object Problem34 {

  def factorial(n : BigInt) : BigInt = factorialTr(n, 1)

  def factorialTr(n : BigInt, result : BigInt) : BigInt = {
    if (n == 0) result
    else factorialTr(n - 1, n * result)
  }

  def main (args : Array[String]) {

    var ls : List[BigInt] = Nil

    for (i <- BigInt(3) to BigInt(1000000)) {

      val xs = i.toString.map(_.toString.toInt).toList

      val fs = xs.map(factorial(_))

      val s = sum(fs)

      if (s == i) {

        println("i = " + i)
        ls = i :: ls
      }
    }

    println("list = " + ls.mkString(", "))
    println("Result = " + sum(ls))
  }

  def sum(ls : List[BigInt]) = ls.foldLeft(BigInt(0L))(_+_)
}
