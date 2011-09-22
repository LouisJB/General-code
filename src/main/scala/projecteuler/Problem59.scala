package projecteuler

import scala.io.Source._


object Problem59 {

  def main(args : Array[String]) {

    var a = 'a'.toInt
    var z = 'z'.toInt

    val lines = loadList("/Users/chillipower_uk/dev/projects/testprojects/training/Training-Scala/src/main/scala/projecteuler/cipher1.txt")

    val ls = lines.head.split(",").toList.map(_.toInt)

//    println("ls = " + ls.size + "," + ls.mkString(", "))

    def crack() = {

      for (c1 <- a to z) {
        for (c2 <- a to z) {
          for (c3 <- a to z) {

            val pt = decode(c1, c2, c3, ls, List[Int]())

            val s = pt.map(_.toChar).mkString
            if (s.toLowerCase().contains("the") && s.toLowerCase().contains("gospel")) {
              println("\n" + c1 + ", " + c2 + ", " + c3 + " : " + s + ", sum = " + pt.sum)
            }
          }
        }
      }
    }

    def decode(c1 : Int, c2 : Int, c3 : Int, as : List[Int], pt : List[Int]) : List[Int] = {

      if (as.size >= 1) {
        val xs = as.take(1)
        //println("decoding: " + xs)
        decode(c2, c3, c1, as.drop(1), (xs(0) ^ c1) :: pt)
      }
      else pt.reverse
    }

    crack()
  }

  def loadList(fileName : String) : List[String] = {

    val lines = fromFile(fileName).getLines().toList // . "\r\n").toList

//    println("Lines " + lines size)
//    println("head " + lines head)

    lines
  }
}
