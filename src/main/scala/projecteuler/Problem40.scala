package projecteuler

// 0.123456789101112131415161718192021...
// d_(1) × d_(10) × d_(100) × d_(1000) × d_(10000) × d_(100000) × d_(1000000
object Problem40 {

  def main(args : Array[String]) {

    //problem40bruteForce
    
    problem40
  }

  def problem40bruteForce {
    var ls = List[Long]()
    var s = new scala.StringBuilder
    var i = 1
    while (s.length < 1000000) {
      
      s = s.append(i)
      i = i + 1
    }

    for (d <- 0 to 6) {
      val p = math.pow(10, d).toInt
      val x = s.charAt(p - 1).toString.toInt
      println("d = " + d + ", x = " + x)
      ls = x :: ls
    }

    println(Utils.product(ls))

    problem40
  }

  def problem40 {
    var ls = List[Long](1)

    var p : Int = 0
    for (d <- 1 to 6) {
      val s = math.pow(10, d).toInt
      val x = (s - p) / d
      val r = x.toInt % d
      val y = x.toString.charAt(r).toString.toInt
      println("x = " + x + ", r = " + r + ", y = " + y)
      ls = y :: ls
      p = p + s * d
      println("x = " + x + ", s = " + s + ", p = " + p)
    }

    println("ls = " + ls.mkString(", "))
    val r = Utils.product(ls)
    println("Product = " + r)
  }
}
