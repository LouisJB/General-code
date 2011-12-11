package projecteuler

import math._
import courses.m381.M381._
import courses.m381.Primes._

object Problem70 {

  def main(args : Array[String]) {
    val m = (10 * 1000 * 1000)
    println("phi(49) = " + fastPhi(49) + ", " + phiOfPrimePairs(7, 7) + ", " + fastPhi(15) + ", " + phiOfPrimePairs(3, 5))
    println("running...")
    def phiOfPrimePairs(p1 : Int, p2 : Int) = (p1-1)*(p2-1)
    def isPerm(n : Int, m : Int) = {
      val ns : String = n.toString
      val ms : String = m.toString
      if (ns.size == ms.size) {
        val xs = ns.toArray.sortWith(_ > _).toList
        val ys = ms.toArray.sortWith(_ > _).toList
        //println("x = " + x + ", ps = " + ps + ", " + xs)
        xs == ys
      }
      else false
    }

    val s = sqrt(m) * 2
    val ps = primeStream.takeWhile(_ < s).toList
    println("max prime " + ps.last)

    val rs =
      for (p1 <- ps;
           p2 <- ps if ((p2 != p1) && ((p1 * p2)< m) && isPerm(p1*p2, phiOfPrimePairs(p1, p2)))
      ) yield (p1 * p2, phiOfPrimePairs(p1, p2))


    val r = rs.map(x => (x._1, x._1.toDouble / x._2.toDouble)).sortWith(_._2 < _._2).head

    // answer is 8319823
    println("ans r = " + r)

    assert(r._1 == 8319823)
    return;




    //methods below of brute force are all waaay too slow
    // but interestingly the compact 'collect' method (2) is faster than
    // even the imperative style (1) below
    var ls = List[(Int, Int)]()
    var start = System.currentTimeMillis()

    (10 to m).foreach{ x =>
      val p = fastPhi(x)
      val ps1 : String = p.toString
      val xs1 : String = x.toString
      if (xs1.size == ps1.size) {
         val ps = ps1.toArray.sortWith(_ > _).toList
         val xs = xs1.toArray.sortWith(_ > _).toList
         //println("x = " + x + ", ps = " + ps + ", " + xs)
         if (xs == ps) ls = (x, p) :: ls
       }
    }

    println("1. ls size = " + ls.size)
    println("1. took " + (System.currentTimeMillis - start) + "ms")


    start = System.currentTimeMillis

    val rs1  = (10 to m by 1).map(x => (x, fastPhi(x))).collect {
      case (x, p) if ((x.toString.size == p.toString.size) &&
        (x.toString.toArray.sortWith(_ > _).toList == p.toString.toArray.sortWith(_ > _).toList)) => (x, p)
    }

    println("2. rs1 size = " + rs1.size)
    println("2. took " + (System.currentTimeMillis - start) + "ms")


    start = System.currentTimeMillis

    val rs2 = (10 to m).map(x => {
      val p = fastPhi(x)
      val ps1 : String = p.toString
      val xs1 : String = x.toString
      if (xs1.size == ps1.size) {
        val ps = ps1.toArray.sortWith(_ > _).toList
        val xs = xs1.toArray.sortWith(_ > _).toList
        //println("x = " + x + ", ps = " + ps + ", " + xs)
        if (xs == ps) Some(x, p) else None
      }
      else None
    }).collect {
      case Some(x) => x
    }

    println("3. rs2 size = " + rs2.size)
    println("3. took " + (System.currentTimeMillis - start) + "ms")

    /*
      case (x, p) => {
        val ps1 : String = p.toString
        val xs1 : String = x.toString

        { case _ if (xs1.size == ps1.size) => {
          val ps = ps1.toArray.sortWith(_ > _).toList
          val xs = xs1.toArray.sortWith(_ > _).toList
          println("x = " + x + ", ps = " + ps + ", " + xs)
          (x, p)
        } }

        new PartialFunction[Int, (Int, Int)] {
          val p = fastPhi(x)
          val ps1 : String = p.toString
          val xs1 : String = x.toString
          val (isDefined, value) =
          if (xs1.size != ps1.size) (false, (0, 0))
          else {
            val ps = ps1.toArray.sortWith(_ > _).toList
            val xs = xs1.toArray.sortWith(_ > _).toList
            println("x = " + x + ", ps = " + ps + ", " + xs)
            (x, p)
            ((xs == ps), (x, p))
          }
          def isDefinedAt(x : Int) = isDefined
          def appy(x : Int) = value
        }
      }
    }}
*/

    val z = rs1.map(x => (x._1, x._1.toDouble / x._2)).sortWith(_._2 < _._2).head

    println("answer:")
    println(rs1.mkString(",\n"))
    println("ans " + z)
  }

  class Memoize1[-T, +R](f: T => R) extends (T => R) {
    import scala.collection.mutable
    private[this] val vals = mutable.Map.empty[T, R]

    def apply(x: T): R = {
      if (vals.contains(x)) {
        vals(x)
      }
      else {
        val y = f(x)
        vals + ((x, y))
        y
      }
    }
  }

  object Memoize1 {
    def apply[T, R](f: T => R) = new Memoize1(f)
  }
}
