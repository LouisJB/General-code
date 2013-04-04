package projecteuler

import Utils._

//The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843)
// and 66430125 (4053). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
object Problem62 {
  def main(args : Array[String]) {
    import scala.collection.mutable.{ Map => MMap }

    val ans = answerWithTime("Problem 62", "minCubeRoot:") {
      def sortedDigits(s : String) = s.toCharArray.map(_.asDigit).sortWith(_ > _).mkString
      val cubesMap : MMap[String, (Int, List[String])] = MMap()
      (100L to 10000).foreach(n => {
        val c = BigInt(n)*n*n
        val sd = sortedDigits(c.toString)
        val entry = cubesMap.getOrElse(sd, (0, Nil))
        cubesMap.put(sd, (entry._1 + 1, c.toString :: entry._2))
      })
      cubesMap.filter{ case (_, (c, _)) => c == 5 }.values.toList.flatMap(_._2).sortWith(_ < _).head
    }
    assertDone(ans.toLong == 127035954683L)
  }
}
