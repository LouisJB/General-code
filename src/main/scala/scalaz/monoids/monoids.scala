package scalaz.monoids

import scalaz._
import Scalaz._

object Monoids {

  implicit def toSome[T](a : Any) = Some(a)

  def main(args : Array[String]) {
    println("testing simple monoid addition")

    case class C(x : Option[Int], y : Option[Int])

    val ls = List(C(some(1), Some(1)), C(None, None), C(Some(5), None))

    val total = ls.map(e => e.x |+| e.y).asMA.sum

    println("total = " + total)
    println("done")
  }
}
