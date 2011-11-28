package projecteuler

import scala.io.Source._

object Problem22 {
  def main(args : Array[String]) {
    val allNamesFile = fromFile("/Users/chillipower_uk/dev/projects/testprojects/training/Training-Scala/resources/euler/names22.txt")
    val allNameLines = allNamesFile.getLines().toList
    val allNames : List[String] = allNameLines.flatMap(_.split(",")).map(_.replaceAll("\"", ""))
    val sortedNames = allNames.sortWith(_ < _)
    val namePositions = sortedNames.zipWithIndex
    
    def nameValue(s : String) =
      s.map(charValue(_)).sum
    
    def charValue(c : Char) =
      c.toInt - 'A'.toInt + 1
    
    println("COLIN = " + nameValue("COLIN"))

    val nameValues : List[Int] = namePositions.map(n => (n._2 + 1) * nameValue(n._1))
    println("ans = " + nameValues.sum)
  }
}
