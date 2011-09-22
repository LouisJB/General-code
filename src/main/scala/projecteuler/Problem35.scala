package projecteuler

import java.lang.Math._

//How many circular primes are there below one million?
object Problem35 {

  def main (args : Array[String]) {

    val r = rotations(123450)
    println(r)

    var count = 0
    for (i : Int <- 2 to 1000000) {

      if (isPrime(i)) {
        val rots = rotations(i)
        //println("i = " + i + ", rots = " + rots)

        val primeRots = rots.filter(n => isPrime(n))

        if (primeRots.length == rots.length) {
          count = count + 1
          println("adding : " + i)
        }
}
    }

    println("count = " + count)
  }

  def isPrime(n: Int) = {
    if (n <=2 ) true
    else (2 to (ceil(sqrt(n))).toInt) forall (n % _ != 0)
  }


  def rotations(item : Int) :List[Int] = {
    val ls = item.toString.map(_.toString.toInt).toList
    var xs : List[Int] = List(item)
    rotations(ls)
  }

  def rotations(ls : List[Int]) = {
    var xs = ls
    val l = ls.length
    if (l > 1) {
      for (i : Int <- 1 to l - 1) {

        val s = rotate(i, ls).mkString
        xs = s.toInt :: xs
      }
    }

    xs
  }

  def rotate2[T](n: Int, l: List[T]): List[T] = l match {
    case Nil => Nil
    case xxs@(x :: xs) => if(n <= 0) xxs else rotate2(n - 1, xs ::: List(x))
  }

  def rotate[T](n: Int, l: List[T]): List[T] = {
    val max = n % l.length;
    if (max < 0) rotate(max + l.length, l)
    else l.drop(max) ::: l.take(max)
  }
}
