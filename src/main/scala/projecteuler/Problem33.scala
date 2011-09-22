package projecteuler

object Problem33 {

  def main(args : Array[String]) {

    findFractions(100)
  }

  def findFractions(n : Int) {

    for (d <- 11 to n) {
      for (n <- 10 to d) {
        if ((d != n) && (n % 10 != 0)) {
          //println("ans: n/d = " + n + " / " + d)

          for (cn <- n.toString) {
            for (cd <- d.toString) {
              if (cn == cd) {
                //println("cd = " + cd + ", cn = " + cn)
                val dds = d.toString.filter(x => !x.equals(cd))
                val nns = n.toString.filter(x => !x.equals(cn))

                if ((dds.length > 0) && (nns.length > 0)) {
                  val dd = dds.toInt
                  val nn = nns.toInt

                  if ((n.toDouble/d.toDouble).equals(nn.toDouble/dd.toDouble)) {

                    println("ans: n/d = " + n + " / " + d + ", nn/dd = " + nn + " / " + dd)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
