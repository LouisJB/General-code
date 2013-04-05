package projecteuler

import Utils._
import utils.RichBigInt._
import courses.m381.M381._

object Problem65 {
  def main(args : Array[String]) {
    val ans = answerWithTime("Problem 65", "100th CF:") {
      val eCfSeq = FCF.eCfSeqGen(100)
      val eFCF = FCF(eCfSeq)
      val cs = eFCF.fcf
      val hundrethTerm = cs.last
      val sum = hundrethTerm.n.digitsSum
      val nearlyE = hundrethTerm()
      val reducedForm = hundrethTerm.reduced
      println("nearly e was : " + nearlyE + ", from term " + reducedForm)
      sum.toString
    }
    assertDone(ans.toLong == 272)
  }
}


