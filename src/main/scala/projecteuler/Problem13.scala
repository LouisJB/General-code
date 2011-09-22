package projecteuler

import scala.io.Source
import java.io.File

object Problem13 {

  def main(args : Array[String]) {

    val file = new File("src/main/scala/projecteuler/problem13.data")
    println(file.getAbsolutePath())

    val lines = Source.fromFile(file).getLines

    val sum = lines.map(x => BigInt(x)).sum

    println(sum.toString.take(10))
  }
}
