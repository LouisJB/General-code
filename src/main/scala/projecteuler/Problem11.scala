package projecteuler

import scala.io.Source
import java.io.File


object Problem11 {

  def main(args : Array[String]) {

    val file = new File("src/main/scala/projecteuler/problem-11.txt")
    println(file.getAbsolutePath())

    val lines = Source.fromFile(file).getLines

    val dataList = lines.map(l => l.split(" ").map(_.toInt)).toArray

    val data = dataList.toArray

    val s = 4
    val w = data(0).size -1
    val h = data.size - 1

    var t = 1
    var max = 0

    for (y <- 0 to h) {
      for (x <- 0 to w - s) {
        for (z <- 0 until s) {
          t *= data(y)(x+z)
        }
        if (t > max) {
          max = t
          println("new max at " + x + "," + y + ", horizontal : " + max)
        }
        t = 1
      }
    }

    for (y <- 0 to h-4) {
      for (x <- 0 to w) {
        for (z <- 0 until s) {
          t *= data(y+z)(z)
        }
        if (t > max) {
          max = t
          println("new max at " + x + "," + y + ", vertical : " + max)
        }
        t = 1
      }
    }

    for (y <- 0 to h-4) {
      for (x <- 0 to w-4) {
        for (z <- 0 until s) {
          t *= data(y+z)(x+z)
        }
        if (t > max) {
          max = t
          println("new max at " + x + "," + y + ", diag : " + max)
        }
        t = 1
      }
    }

    for (y <- s to h) {
      for (x <- 0 to w-4) {
        for (z <- 0 until s) {
          t *= data(y-z)(x+z)
        }
        if (t > max) {
          max = t
          println("new max at " + x + "," + y + ", diag-1 : " + max)
        }
        t = 1
      }
    }

    println(data)
  }
}
