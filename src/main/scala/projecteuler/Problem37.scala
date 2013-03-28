package projecteuler

import scala.math._

//Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
// answer: 739397,3797,3137,797,373,317,313,73,53,37,23 = 748317
object Problem37 {

  def main(args : Array[String]) {

    println(isPrimeF(BigInt(3797)))

    val test = getSubdigits(toList(BigInt(123456)))
    println(test.mkString(", "))
    println("test r = " + checkSubdigits(101L))

    var ls = List[BigInt]()
    for (n <- BigInt(11) to BigInt(1000000000) by BigInt(2L)) {
      if (n % 1000001 == BigInt(0)) println(n)
      //val l = n % 10
      //if (isPrime(l.toInt)) {
      //	if (isPrime(n)) {
                      //println(n)
                      if (checkSubdigits(n)) {
                              ls = n :: ls

                              println(ls.mkString(","))
                              println("result = " + ls.length + ", sum = " + sum(ls))
                      if (ls.size >= 11) util.control.Breaks.break
                      }
      //	}
      //}
    }
    println(ls.mkString(","))
    println("result = " + sum(ls))
  }

  def checkSubdigits(n : BigInt) = {

    val digits = toList(n)

    val subdigits = getSubdigits(digits)
    //println("list = " + subdigits.mkString(", "))
    var res = subdigits.forall(y => {
            val x = BigInt(y.mkString)
            isPrime(x)
            //println("checking n = " + n + ", res = " + res)
    })

    res
  }

  def getSubdigits(ls : List[BigInt]) : List[List[BigInt]] = {
    val subdigits1 = getSubdigitsR(ls) //.sortWith(_.size > _.size)
    val subdigits2 = getSubdigitsR(ls.reverse).map(_.reverse)
    //val xs = subdigits1 ::: subdigits2

    // zip the two lists together to maintain same ordering
    val xs = (subdigits1, subdigits2).zipped flatMap(List(_,_))

    // reverse so that shortest are first
    xs.distinct.reverse
  }

  def getSubdigitsR(ls : List[BigInt]) : List[List[BigInt]] = {
    ls match {
            case Nil => Nil
            case _ => ls :: getSubdigitsR(ls.drop(1))
    }
  }

  def sum(ls : List[BigInt]) = ls.foldLeft(BigInt(0))(_+_)

  def toList(n : BigInt) : List[BigInt] = n.toString.map(x => BigInt(x.toString)).toList

  def isPrimeF(n : Long) = {
    if (n < 2) false
    else if ((n==2) || (n==3) || (n==5) || (n==7)) true
    else if ((n==4) || (n==6) || (n==8) || (n==9)) false
    else (2 to (ceil(sqrt(n))).toInt) forall (n % _ != 0)
  }

  def isPrimeF(n : BigInt) = {
    if (n < BigInt(2)) false
    else if ((n==BigInt(2)) || (n==BigInt(3)) || (n==BigInt(5)) || (n==BigInt(7))) true
    else if ((n==BigInt(4)) || (n==BigInt(6)) || (n==BigInt(8)) || (n==BigInt(9))) false
    else (BigInt(2) to sqrtBI(n)) forall (n % _ != BigInt(0))
  }

  val primes = scala.collection.mutable.Map[BigInt, Boolean]()

  def isPrime(n : BigInt) : Boolean = {
    if (!primes.contains(n)) {
      val x = isPrimeF(n)

      if (n < 1000000) {
        primes.put(n, x)
      }
      x
    }
    else {
      //println("from hash")
      primes.getOrElse(n, false)
    }
  }

  def sqrtBI(n:BigInt) = {
    def next(guess:BigInt) = (guess + n / guess) / 2
    Stream.iterate(n)(next).find(square(_) <= n).get
  }

  def square(n : BigInt) = n * n
}
