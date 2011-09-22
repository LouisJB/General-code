package projecteuler

// 28433×2^(7830457)+1
object Problem97 {

  def main (args : Array[String]) {

    val x = calc(10)
    
    println("Ans = " + x)
  }

  def calc(n : Int) : BigInt = {

    val x = BigInt(2).modPow(7830457, BigInt(10).pow(n))
    val y = x * BigInt(28433) + 1
    val s = y.toString

    BigInt(s.toList.drop(s.length - 10).mkString)
  }
}
