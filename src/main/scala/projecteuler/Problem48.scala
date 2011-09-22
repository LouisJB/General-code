package projecteuler

object Problem48 {

  def main(args : Array[String]) {

    val xs = calc(1000)
    val s = xs.toString.map(c => c.toString)
    val y = s.drop(s.length - 10).mkString
    println("Result = " + y)
  }

  def calc(n : Int) = {

    val xs = (1 to n).map(x => BigInt(x).pow(x))

    xs.foldLeft(BigInt(0))(_ + _)
  }
}
