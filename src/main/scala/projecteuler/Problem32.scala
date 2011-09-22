package projecteuler

object Problem32 {

  def main(args : Array[String]) {

    //println("213456789 " + isPandigital(213456789L))
    //println("345678912 " + isPandigital(345678912L))
    //println("3456789121 " + isPandigital(3456789121L))
    //println("34567891222 " + isPandigital(34567891222L))

    var ls = List[BigInt]()

    for (i <- 1 to 10000) {
      for (j <- 1 to 10000) {

        val x = i * j

        val r = BigInt(i.toString + j.toString + x.toString)

        if (r.toString.length == 9) {

          if (Utils.isPandigital1_9(r)) {
            println(x + ", (" + i + "*" + j + ")")
            ls = BigInt(x) :: ls
          }
        }
      }
    }

    ls = ls.distinct
    val s = Utils.sum(ls.map(_.toLong))

    println("sum = " + s)
  }
}
