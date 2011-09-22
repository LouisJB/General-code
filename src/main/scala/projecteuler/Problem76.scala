package projecteuler

object Problem76 {

  def pent(n : BigInt) : BigInt = (BigInt(3) * (n*n) - n) / BigInt(2)

  val map = scala.collection.mutable.Map[(Short, Short), BigInt]()

  // problem 76 part(1. 100) = part(100) - 190569292 - 1
  def part(min : Short, n : Short) : BigInt = {
    if (min > n) 0
    else if (min == n) 1
    else {
      val x = map.getOrElseUpdate(((min + 1).toShort, n), part((min + 1).toShort, n))
      val y = map.getOrElseUpdate((min, (n - min).toShort), part(min, (n - min).toShort))
      x + y
    }
  }

  def main(args : Array[String]) {

    (1 to 100).foreach(n => println("n = " + n + ", part = " + part(1, n.toShort)))
    println(part(1, 100) - 1)
  }
}