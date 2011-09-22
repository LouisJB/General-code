package projecteuler

import scala.io.Source._

object Problem42 {
  
  def wordValue (word : String) : Long = {
    val s = word.filter(_ != '"') // remove " chars
    val v = s.foldLeft(0)((s, l) => (s + (l - 'A' + 1)))
    println("wvalue word = " + word + ", s = " + s + ", v = " + v)
    v
  }

  def tri (n : Long) = (n * (n + 1)) / 2

  def main(args : Array[String]) {

    println("Test SKY = " + wordValue("SKY"))
    println("Test A = " + wordValue("A"))
    println("Test ABOUT = " + wordValue("ABOUT"))

    val words = readwords("/Users/chillipower_uk/dev/projects/testprojects/training/Training-Scala/src/projecteuler/words.txt")
    println("Number of words = " + words.length + ", words = " + words.mkString(","))
    
    val values = words.map(wordValue)
    println("values.length = " + values.length + ", values = " + values.mkString(","))

    println(
      values.filter(v => (v == tri(math.sqrt(2 * v).toLong))).length)
  }

  def readwords(fileName : String) /* : List[Seq[Char]] */ = {

    val lines = fromFile(fileName).getLines() //",").toList

    println(lines.mkString(","))
    lines.map(_.toUpperCase)
  }
}
