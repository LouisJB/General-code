package projecteuler

object Problem1 extends App {

  val r = (3 until 1000).filter(x => x % 3 == 0 || x % 5 == 0).sum

  println("Ans : " + r)
}
