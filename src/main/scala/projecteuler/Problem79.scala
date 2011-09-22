package projecteuler

import scala.io.Source._

object Problem79 {

  def main(args : Array[String]) {

    // load the list in
    val ls = loadList("/Users/chillipower_uk/dev/projects/testprojects/training/Training-Scala/src/projecteuler/keylog.txt")
    
    // test list (result should be something like: 648350)
    //val ls = List("435", "850", "630", "645")

    println(ls.mkString(","))

    var rs = List[Int]()
    for (i  <- 0 to 2) {  // for each position
      for (s <- ls) {     // for each string in the list
        var x = s.toList(i).toString.toInt // convert the digit string to a List[Int]
        if (!rs.contains(x)) {    // if the digit is not already included
          val r = findMax(x, ls)  // find the longest prefix that contains the digit
          rs = rs ::: r           // append the match into the result list
        }
      }
    }
    
    rs = rs.distinct  // there will be some duplicates, remove them
    println(rs.mkString)
  }

  // look through the list of strings and return the longest
  // substring that contains digit in rightmost position as a List[Int]
  def findMax(x : Int, ls : List[String]) : List[Int] = {
    
    var rs = List[Int](x)
    var max = 0
    
    for (xs <- ls) {
      val is = xs.map(_.toString.toInt).toList
      val p = is.indexOf(x)
      
      if (p > max) {
        max = p
        rs = is.take(p + 1)
      }
    }
    
    rs
  }

  def loadList(fileName : String) : List[String] = {

    //"foo\n".stripLineEnd == "foo"
    val lines = fromFile(fileName).getLines().toList // . "\r\n").toList

    println("Lines " + lines size)
    println("head " + lines head)

    lines
  }
}
