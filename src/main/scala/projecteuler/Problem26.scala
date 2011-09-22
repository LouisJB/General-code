package projecteuler

object Problem26 {

  def main(args : Array[String]) {

    println(findLongest(1000))
  }

  // http://eli.thegreenplace.net/2009/02/25/project-euler-problem-26/
  // http://pavelfatin.com/scala-for-project-euler/#more-177
  def findLongest(n : Int) : Int = {

    val ps = (2 until n).map(i => (1 to 2000)
         .find(BigInt(10).modPow(_, i) == 1))

    val r = 2 + ps.indexOf(Some(ps.flatten.max))
    r
  }
}
