package projecteuler

object Problem52 {

  def main(args : Array[String]) {

    val n = problem52(6)
    println("result = " + n)
  }

  def problem52(n : Int) : BigInt = {
    for (n <- BigInt(1) to BigInt(1000000)) {
      var xs = List[List[BigInt]]()
      for (m <- 1 to 6) {
        val x = n * m
        xs = toList(x).sortWith((x, y) => x < y) :: xs
      }
      val r = check(xs)
      if (r) {
        println("result list = " + xs.mkString(", "));
        return n;
      }
    }
    BigInt(0)
  }

  def check(ls : List[List[BigInt]]) = {
    //ls.distinct.size == 1
    ls.tail.forall(_ == ls.head)
  }

  def toList(n : BigInt) : List[BigInt] = n.toString.map(x => BigInt(x.toString)).toList
}
