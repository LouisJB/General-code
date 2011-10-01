#!/bin/sh
exec scala "$0" "$@"
!#

import scala.math._

object CheckDigit {

  def checkDigit(m : Int)(s : String) =
    s.map(c => c.asDigit).zipWithIndex.map(x => x._1 * (x._2 + 1)).sum % m

  def main(args : Array[String]) {
  
    val s = args(0)
    val checkDigit11 = checkDigit(11) _
    var r = checkDigit11(s)
    
    println("check digit for " + s + " = " + r)
  }
}

CheckDigit.main(args)

