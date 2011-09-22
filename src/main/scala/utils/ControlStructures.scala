package utils

object ControlStructures {

  def main(args: Array[String]) {

    var x = 0

    While (x < 20) { 
      x += 1
      println(x)
    }
  }

  def While(cond: => Boolean)(body: => Unit) {

    if (cond) {
      body
      While(cond)(body)
    }
  }
}
