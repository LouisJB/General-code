package projecteuler

import java.lang.Math._
import scala.annotation.tailrec

object Utils {

  def fix[A,B](f: (A=>B)=>(A=>B)): A=>B = f(fix(f))(_)

  // recursive
  //@tailrec
  def factorialR(n : Int) : Long = if (n == 1) 1 else n * factorial(n-1)

  // using reduce
  def factorial(n : Int) = (2 to 8).reduceLeft(_*_)

  // calc nCr
  def combinations(n : Long, k : Long) : BigInt = {

    var res = BigInt(1L);

    for (i <- 1L to k) {

      res = res * (n - i + 1)
      res = res / i
    }

    res
  }
  
  def sum(ls : List[Long]) = ls.foldLeft(0L)(_+_)
  def sum(ls : List[BigInt]) = ls.foldLeft(BigInt(0))(_+_)

  def product(ls : List[Long]) = ls.foldLeft(1L)(_*_)

  def maximize[A, B <% Ordered[B]](ds: Iterable[A])(fn: A => B): A =
    ds.map(a => (a, fn(a)))
      .reduceLeft((p1,p2) => if(p2._2 > p1._2) p2 else p1)
      ._1
  
  def minimize[A, B <% Ordered[B]](ds: Iterable[A])(fn: A => B): A =
    ds.map(a => (a, fn(a)))
      .reduceLeft((p1, p2) => if(p2._2 < p1._2) p2 else p1)
      ._1

  def perm[T](n: Int, l: List[T]): List[List[T]] =
    n match {
      case 0 => List(List())
      case _ => for(el <- l;
                  sl <- perm(n-1, l filter (_ != el)))
              yield el :: sl
  }

  def permuations[T](list : List[T]) : List[List[T]] = list match {
    case Nil => List(List())
    case list => list flatMap ((elem: T) => permuations[T](list filterNot ((a: T) => a == elem)) map ((b: List[T]) => elem :: b))
  }

  // permutation stream (note: not efficient on memory for large permutation sets)
  def permute[A](xs: List[A]): Stream[List[A]] = xs match {
    case Nil => Stream(Nil)
    case _ => xs.toStream.flatMap(x => permute(xs.filter(_ != x)).map(x :: _))
  }
  
  /*
  def perm[T](ls : List[T]) : List[T] = {

    ls match {
      case Nil => Nil
      case n :: xs => xs.head :: n :: xs.tail
    }
  }*/

  def isPalindrome(n : Long) : Boolean = {

    val s = n.toString
    isPalindrome(s)
  }

  def isPalindrome(n : BigInt, radix : Int) : Boolean = {

    val s = n.toString(radix)
    isPalindrome(s)
  }

  def isPalindrome(s : String) : Boolean = {

    val f = s.toList
    val r = s.toList.reverse

    //println("testing " + f.mkString + " and " + r.mkString)
    f.equals(r)
  }

  def isPandigital1_9(n : BigInt) : Boolean = {

    isPandigital(n, (1 to 9).toList)
  }

  def isPandigital0_9(n : BigInt) : Boolean = {

    isPandigital(n, (0 to 9).toList)
  }

  def isPandigital(n : BigInt, ls : List[Int]) : Boolean = {

    if (n.toString.length != ls.length) return false
    
    val s = n.toString
    val xs = s.map(_.toString.toInt)
    //println("xs = " + xs.length + ", " + xs.mkString(", "))

    for (i <- ls) {
      val rs = xs.filter(_.equals(i))
      //println("rs = " + rs.length + ", " + rs.mkString(", "))
      if (rs.length != 1) return false
    }

    true
  }
}
