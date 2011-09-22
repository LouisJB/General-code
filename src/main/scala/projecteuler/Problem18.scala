package projecteuler

import scala.io.Source._

case class Tree[+T](value: T, left: Option[Tree[T]], right: Option[Tree[T]])

//http://www.scala-lang.org/archives/downloads/distrib/files/nightly/docs/library/index.html
object Problem18 {
  
  def main(args : Array[String]) {

    //val tree = loadTree("/Users/chillipower_uk/IdeaProjects/TestScala1/src/Problem18.txt")

    val tree = loadTree("/Users/chillipower_uk/dev/projects/testprojects/training/Training-Scala/src/euler/triangle.txt")

    val max = findMax(tree)

    println("max = " + max)
  }

  def loadTree(fileName : String) : Option[Tree[Int]] = {
	
	  //"foo\n".stripLineEnd == "foo"
	val lines = fromFile(fileName).getLines() //"\n")
            .map(_.split(Array(' ', '\r', '\n'))
            .view.map(_.toInt).toList).toList

    println("Lines " + lines size)
    println("head " + lines(0).head)

    Some(lines.foldRight(Iterator.fill(lines.last.length + 1)
              (None:Option[Tree[Int]]).toList) {
                _.iterator zip _.iterator.sliding(2) map {
                  case (v,ch) => Some(Tree(v, ch.head, ch.last))
                } toList
            }.head.get)
  }
  
  def findMax(t : Option[Tree[Int]]) : Int = t match {

    case None => 0
    case Some(x) =>
      math.max(findMax(x.left),
    		  findMax(x.right)) + x.value
  }
}
