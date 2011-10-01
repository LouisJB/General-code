#!/bin/sh
exec scala "$0" "$@"
!#

import scala.math._
import scala.annotation.tailrec
import scala.util.Random

object M381 {

  import Functions._
  import Sorting._

  lazy val fibs: Stream[Int] = Stream.cons(0, Stream.cons(1, fibs.zip(fibs.tail).map(p => p._1 + p._2)))
  lazy val triangles : Stream[Int] = 0 #:: triangles.zipWithIndex.map(p => p._1 + p._2 + 1)

  def gcd(a : Int, b : Int) : Int = if (b == 0) a else gcd(b, a % b)
  def gcdl(ls : List[Int]) : Int = ls.foldLeft(ls.head)((a : Int, b : Int) => gcd(a, b))

  // theorem 4.8, lcm(a, b) x gcd(a, b) = ab
  def lcm(a : Int, b : Int) : Int = (a * b) / gcd(a, b)
  def lcml(ls : List[Int]) : Int = ls.distinct.foldLeft(1)(_ * _) / gcdl(ls)

  // euler totient (phi(n)) function, number of numbers less than n that are co-prime to n
  def totient(n : Int) = (0 until n).filter(x => gcd(x, n) == 1).size
  
  def tau(n : Int) = (1 to n).filter(x => (n % x) == 0).size
  def sigma(n : Int) = (1 to n).filter(x => (n % x) == 0).sum
  def phi(n : Int) = (1 to n).filter(a => gcd(a, n) == 1).size
  def residues(n : Int) = (0 to n - 1)
  def lpresidues(n : Int) = ((-1 * (n/2)) to n / 2)
  def reducedResidues(n : Int) = (1 to n).filter(a => gcd(a, n) == 1)

  trait QuadraticCharacters
  case class QuadraticCharacter(x : Int, p : Int) extends QuadraticCharacters
  case class NonQuadraticCharacter(x : Int, p : Int) extends QuadraticCharacters

  // by Euler's Criterion
  def quadraticCharacter(p : Int) = (1 to p).map(x => if (pow(x, ((p-1)/2)) % p == 1) QuadraticCharacter(x, p) else NonQuadraticCharacter(x, p))

  def quadraticResidue(x : Int, p : Int) = (1 to p).find(a => (x * x) % p == a % p)
  def quadraticResidues(p : Int) = (1 to p-1).map(x => quadraticResidue(x, p)).toList.flatten.distinct.sortWith(_ < _)

  def solveQuadratic(a : Int, b : Int, c : Int) : (Double, Double) = { val discr = (b*b - 4 * a * c); ((-1 * b + sqrt(discr)) / (2 * a), (-1 * b - sqrt(discr)) / (2 * a)) }

  def congruent(a : Int, b : Int, n : Int) = (a - b) % n  == 0

  // least absolute residue
  def lar(a : Int, n : Int) = if (a % n <= abs((a % n) - n)) a % n else (a % n) - n

  // Theorem 2.1 - the order of integer
  def order(a : Int, p : Int) = if (gcd(a, p) == 1) (1 to p).find(x => pow(a, x) % p == 1) else None

  // Theorem 2.2 - length of the decimal cycle for 1/p
  def primeDecimalCycleLen(p : Int) = if (p == 2 || p == 5) 1 else order(10, p)

  // Fermat's little theorem
  // FLT test for a^p-1 congruent 1 (mod p)
  def flt(a : Int, p : Int) : Boolean = BigInt(a).pow(p-1) % p == 1

  // FLT test for prime candidate p, test flt holds for all 1 < a < p && gcd(a, p) = 1
  def fltTest(p : Int) : Boolean = (2 to p-1).filter(x => gcd(x, p) == 1).forall(a => flt(a, p))

  def charmichaelsTo(n : Int) = (2 to n).filter(!isPrime(_)).filter(x => fltTest(x))

  def isPrime(p : Int) = (2 to p-1).forall(a => p % a != 0) // crude is prime test

  def perfect(x : Int) = sigma(x) == 2 * x

  def order2(a : Int, m : Int) = (1 to m).find(c => pow(a, c) % m == 1)
  def orders(n : Int) = (1 to n-1).map(x => (x, order(x, n))).collect{ case a @ (x, Some(y)) => a}

  def primativeRoots(m : Int) = (1 to m).map(n => if (order(n, m) == Some(phi(m))) Some(n) else None).flatten
  def noPrimativeRoots(n : Int) = phi(phi(n))


  // continued fractions (Theorem 1.2 NT unit 7)
  case class FCF(ls : List[Int]) { 
    override def toString = "[%s]".format(ls.mkString(", "))
    def fcf = fcfc(ls)
    def apply() : Double = fcf(ls).reverse.headOption.map(x => Convergent(x._1, x._2)()).getOrElse(0.0)
    
    def fcf(ls : List[Int]) = {
      def fcf2(ls : List[Int], rs : List[(Int, Int)]) : List[(Int, Int)] = {
        println("%s, %s".format(ls.mkString(","), rs.mkString(",")))
        rs match {
          case Nil => ls match {
            case Nil => rs
            case h :: t => fcf2(t, (h, 1) :: rs)
          }
          case h :: Nil => ls match {
            case Nil => rs
            case h2 :: t => fcf2(t, (h2 * h._1 + 1, h2) :: rs)
          }
          case h :: hh :: t => ls match { 
            case Nil => rs
            case h2 :: tail => fcf2(tail, (h2 * h._1 + hh._1, h2 * h._2 + hh._2) :: rs)
          }
        }
      }
      fcf2(ls, Nil).reverse
    }
    def fcfc(ls : List[Int]) : List[(Int, Convergent)] = fcf(ls).zipWithIndex.map(e => (e._2 + 1, Convergent(e._1._1, e._1._2)))
  }
  object FCF {
    implicit def toFCF(ls : List[Int]) = FCF(ls)
  }
  case class Convergent(n : Int, q : Int) {
    override def toString() = "%s/%s".format(n, q)
    def apply() : Double = n.toDouble / q.toDouble
  }
  
  def cf(ls : List[Double]) : Double = ls match { case Nil => 0; case h :: t => h + 1/cf(t) }
  def fcf(a : Int, b : Int) : List[Int] = if (b == 0) Nil else (a/b) :: fcf(b, a % b)
  def fcf2(a : Int, b : Int) : FCF = if (b == 0) FCF(Nil) else FCF((a/b) :: fcf2(b, a % b).ls)
  

  // rational class
  case class R(n : Int, d : Int) {
    private def fcf(a : Int, b : Int) : List[Int] = if (b == 0) Nil else (a/b) :: fcf(b, a % b)
    def fcf : List[Int] = fcf(n, d)
  }
  case object R {
    def apply(t : (Int, Int)) : R = R(t._1, t._2)
  }
  implicit def toRational(t : (Int, Int)) = R(t._1, t._2)

  // prime factor class
  case class PF(n : Int, e : Int) {
    override def toString = "%s^%s".format(n, e)
  }
  case object PF {
    def apply(v : (Int, Int)) : PF = PF(v._1, v._2)
  }
  implicit def toPrimeFactor(v : (Int, Int)) = PF(v._1, v._2)
  implicit def toPrimeFactors(ls : List[(Int, Int)]) : List[PF] = ls.map(e => PF(e._1, e._2))
  implicit def pfsToString(ls : List[PF]) : RichPFList = RichPFList(ls) 
  
  case class RichPFList(ls : List[PF]) {
    override def toString = "[%s]".format(ls.mkString(", "))
    def asString = toString
  }
  
  case class RichInt(n : Int) { def isSquare = { val r = sqrt(n).toInt; r*r == n  } }
  implicit def toRichInt(n : Int) = RichInt(n)
  
  trait LegSym
  case object NullLegendreSymbol extends LegSym
  case class LegendreSymbol(p : Int, q : Int, sign : Int = 1) extends LegSym {
    def alternate = 
      if ((p % 4 == 1) || (q % 4 == 1)) LegendreSymbol(q, p, sign)
      else if ((p % 4 == 3) && (q % 4 == 3)) LegendreSymbol(q, p, -1 * sign)
      else this
    def reduce = if ((p < 0) && (p < q) && (p.isSquare)) LegendreSymbol(1, 1)
      else if (p >= q) LegendreSymbol(p % q, q, sign) 
      else this
    override def toString = "%s(%d, %d)".format(signToString, p, q)
    def signToString = if (sign == -1) "-" else ""
  }

  def solve(s : LegendreSymbol, symbols : List[LegendreSymbol]) : LegSym = {
    println("solving with " + s + ", sym list = " + symbols.mkString(", "))
    s match {
       case LegendreSymbol(p, _, _) if p.isSquare => println("foundsol1"); s
       case a if symbols.contains(a) => NullLegendreSymbol
       case _ => 
         s.reduce match {
           case a @ LegendreSymbol(p, _, _) if p.isSquare => println("found sol2, p = " + p); a
           case a if symbols.contains(a) => println("applying alternate to s");  solve(s.alternate, s :: symbols)
           case a => println("applying alternate to a"); solve(a.alternate, a :: symbols)
        }
      }
  }

  // extract largest prime factors that can form a squared term of the original integer
  def extractLargestSquares(ls : List[PF]) : List[PF] = ls match { 
    case Nil => Nil
    case x :: xs if (x.e >= 2) => (x.n, x.e/2) :: extractLargestSquares(xs)
    case x :: xs => extractLargestSquares(xs)
  }
  
  def removeFactors(ls : List[PF], rs : List[PF]) : List[PF] = ls match {
    case Nil => Nil
    case x :: xs => rs match {
      case Nil => Nil
      case t :: ts if t.n == x.n => if (x.e - t.e > 0) (x.n, x.e - t.e) :: removeFactors(xs, ts) else removeFactors(xs, ts)
      case t :: ts => if (x.n > t.n) removeFactors(ls, ts) else x :: removeFactors(xs, rs)
    }
  }

  // square prime factors
  def square(ls : List[PF]) = ls.map(e => (e.n, e.e * 2))

  def gk(k : Int) = pow(3.0/2.0, k).toInt + pow(2.0, k) - 2

  def isSquare(n : Int) = { val x = sqrt(n).toInt; x*x == n } 

  // find decomposition of n into squares (by 4-square theorem, maximum of 4 squares are required)
  def findSquares(n : Int) : List[Int] =
    n match {
      case 0 => Nil
      case 1 => 1 :: Nil
      case x if isSquare(x) => x :: Nil
      case x => {
        //println("%d".format(x))
        //val v = sqrt(n).toInt; v*v :: findSquares(n-v*v)
        (sqrt(n).toInt to 1 by -1).map(z => z*z :: findSquares(n-z*z)).minBy(_.size)
      }
    }


  def main(args : Array[String]) {
    import PF._
    def checkPrimeFactorSquareReduction() {
      val ls = PF(2, 7) :: PF(3, 4) :: PF(5, 1) :: PF(13, 3) :: Nil
      val rs = extractLargestSquares(ls)
      val xs = removeFactors(ls, square(rs))
      println("%s extracted %s leaving %s".format(ls.asString, rs.asString, xs.asString))
    }
    checkPrimeFactorSquareReduction()

    def checkFourSquares() {
      println("Finding four square decompositions")
      (1 to 25).map(n => (n, findSquares(n))).foreach(println)
    }
    checkFourSquares()

    import BasicStats._
    import Distributions._
    def stats() {
      val stdNormal = normalStream(1.0, 1.0)

      val ls = stdNormal.take(1000).toList
      val v = variance(ls)
      println("variance %f, stddev %f, min %f, ave %f, max %f".format(v, standardDeviation(ls), ls.min, average(ls), ls.max))
    }
    stats()

    import FCF._
    println("fcf convergents: " + List(3, 3, 1, 2, 5).fcf)

    val s = LegendreSymbol(134, 229, -1)
    println("> " + s.alternate.reduce.alternate.reduce)
    println("\nsolving %s = %s".format(s, solve(s, List())))
    
    System.exit(0)

    println("fib(10) = " + fib(10))

    val ls1 = List((6, 10), (6, 15), (6, 18), (6, 24), (6, 21), (2, 21), (12, 24), (232, 18), (41, 11))
    val xs1 = List(5, 3, 6, 7, 12, 1, 2)

    println("quicksortI = " + quicksortI(Array(5, 7, 3, 5, 2, 3, 1, 9)).toList.mkString(", "))
    println("sort = " + sort(List(5, 7, 3, 5, 2, 3, 1, 9)))
    println("merge = " + mergeSort(xs1).mkString(", "))
    
    val ls2 = ls1.map(t => (t, gcd(t._1, t._2)))
    val ls3 = ls1.map(t => (t, lcm(t._1, t._2)))

    println("gcds = \n" + ls2.map(e => "gcd (" + e._1._1 + ", " + e._1._2  + ") = " + e._2).mkString("\n"))
    println("lcms = \n" + ls3.map(e => "lcm (" + e._1._1 + ", " + e._1._2  + ") = " + e._2).mkString("\n"))
    println("fibs = " + fibs.take(20).toList)
    println("triangles = " + triangles.take(20).toList)

    println((1 to 20).map(n => (n, primativeRoots(n), phi(n), phi(phi(n)))).toList.map(e => "n = %d, primativeRoots = %s, phi = %d, phi(phi(n) = %d".format(e._1, e._2, e._3, e._4)).mkString("\n"))

    import BasicStats._
    val ls = List(2, 4, 4, 4, 5, 5, 7, 9).map(_.toDouble)
    println("average of %s is %f".format(ls.mkString(", "), average(ls)))

    println("Standard deviation of %s is %f".format(ls.mkString(", "), average(ls)))
  }
}

object BasicStats {
  import Numeric.Implicits._
  def average[T : Numeric](ls : List[T]) : Double = ls.sum.toDouble / ls.size.toDouble
  def standardDeviation[T : Numeric](ls : List[T]) : Double = pow(ls.map(x => pow((x.toDouble - average(ls)), 2)).sum / ls.size, 0.5)
  def variance[T : Numeric](ls : List[T]) = ls.map(x => pow((x.toDouble-average(ls)), 2)).sum/ls.size
  def stats(ls : List[Double]) = "size %d min %f max %f mean %f variance %f standard deviation %f".format(ls.size, ls.min, ls.max, average(ls), variance(ls), standardDeviation(ls))

  def filter(ls : List[Double], sd : Double) = { val a = average(ls); ls.filter(x => pow(x-a, 2.0) <= sd) }

  def distrib(s : Double, e : Double, d : Double, ls : List[Double]) = (s to e by d).map(r => (r, ls.filter(x => x >= r && x < r + d).size.toDouble/((e-s)/d)))
}

object Distributions {
  def normalStream(mean : Double, sd : Double) : Stream[Double] = Stream.continually(mean + sd * Random.nextGaussian)
  val stdNormal = normal(0, 1) _
  def normal(u : Double, v : Double)(x : Double) = 1/pow(2 * Pi * v, 0.5) * pow(E, -(pow(x-u, 2) / (2 * v)))

  def getUniRnd(n : Int):Double = (1 to n).map(x => Random.nextDouble*6.0-3.0).sum / n
}

object BoxMuller {
  def Z1(u1 : Double, u2 : Double) = pow(-2.0 * log(u1), 0.5) * sin(2.0 * Pi * u2)
  def Z0(u1 : Double, u2 : Double) = pow(-2.0 * log(u1), 0.5) * cos(2.0 * Pi * u2)
  def getBoxMuller() = { val u1 = Random.nextDouble; val u2 = Random.nextDouble; Z0(u1, u2) }
}

object Quadratics {
  def mkQuadratic(a : Int, b : Int, c : Int)(x : Int) = a * x * x + b * x + c
  def mkQuadratic2(a : Int, b : Int, c : Int)(x : Int, y : Int) = a * x * x + b * y * y + c
}

object Primes {

  def pSieve(s : Stream[Int]) : Stream[Int] = { s.head #:: pSieve(s.filter(_ % s.head > 0)) }

  val primeStream = pSieve(Stream.from(2))

  def pf(ps : Stream[Int])(n : Int) : List[Int] = {
    val p = ps.head
    if (n < p) Nil
    else if ((n % p) == 0) p :: pf(ps)(n/p)
    else pf(ps.drop(1))(n)
  }

  val pf1 = pf(primeStream) _
  def primeDecomposition(n : Int) = pf1(n).groupBy(x => x).mapValues(x => x.size)

  // return a list of prime factors...
  def primeFactors(n : Int) : List[Int] = {

    var primeFactors = List[Int]()

    var x = n
    var i = 1
    for (i <- 2 to (x / i) if x > 1) {
      while (x % i == 0) {
        primeFactors = i :: primeFactors
        x = x / i
      }
    }

    primeFactors.reverse
  }

  def primeFactorPowers(n : Int) : List[(Int, Int)] =
    primeFactors(n).groupBy(identity).map{case (x, y) => (x, y.length)}.toList.reverse
  
  def divisorLists(xs : List[(Int, Int)], ys : List[Int]) : List[List[Int]] = xs match {
    case (a, b) :: zs => if (b > 0) divisorLists((a, b - 1) :: zs, a :: ys) ::: divisorLists(zs, ys) else divisorLists(zs, ys)
    case _ => List(ys)
  }

  def divisors(n : Int) = divisorLists(primeFactorPowers(n), List()).filter(_.size > 0).map(e => e.reduceLeft(_ * _))

  def pi(n : Int) = primeStream.takeWhile(x => x < n).size
  
  def main(args : Array[String]) {
    println("primes " + primeStream.take(100).toList.mkString(", "))
    println("primeFactors = " +  primeFactors(args(0).toInt))
    println("primeFactorPowers = " +  primeFactorPowers(args(0).toInt))
    println("divisorLists = " + divisorLists(primeFactorPowers(args(0).toInt), List()))
    println("divisors = " + divisors(args(0).toInt))
    println("pi n " + (2 to 4).map(x => { val y = math.pow(10, x).toInt; (y, pi(y)) } ))
  }
}

object Functions {

  def fib(n : Int) = fibTr(n, 1, 0)
  @tailrec def fibTr(n: Int, a: Int, b: Int): Int = n match {
    case 0 => println("b = " + b); b
    case _ => println("n=" + n + ", a+b = " + (a+b) + ", a = " + a + ", b = " + b); fibTr(n-1, a+b, a)
  }
}

object Probability {
  def perm[T](ls : List[T], ys : List[T]) : List[List[T]] = ls match {
    case Nil => List(ys)
    case (x :: xs) =>  perm(xs, x :: ys) ::: perm(xs, ys)
  } 
}

object Sorting {

  def mergeSort(ls : List[Int]) : List[Int] = ls match {
    case x :: Nil => ls
    case _ => {
      val (a, b) = ls.splitAt(ls.length / 2)
      merge(mergeSort(a), mergeSort(b))     
    }
  }
 
  def merge(xs : List[Int], ys : List[Int]) : List[Int] = xs match {       
    case Nil => ys
    case _ => {
      ys match {
        case Nil => xs
        case z :: zs =>
          if (z < xs.head)
            z :: merge(zs, xs)
          else
            xs.head :: merge(ys, xs.tail)               
      }   
    }
  }

  // quick sort
  def sort(xs: List[Int]): List[Int] = {
    if (xs.length < 2)
      xs
    else {
      val pivot = xs(xs.length / 2)
      sort(xs.filter(_ < pivot)) :::
           xs.filter(_ == pivot) :::
           sort(xs.filter(_ > pivot))
    }
  }

  def quicksortI(ar : Array[Int]) : Array[Int] = quicksortI(ar, 0, ar.length - 1)

  // quick sort imperatively
  def quicksortI(ar : Array[Int], p : Int, q : Int) : Array[Int] = {
    if (p < q) {
      val pivot = partI(ar, p, q)
      quicksortI(ar, p, pivot - 1)
      quicksortI(ar, pivot + 1, q)
    }
    ar
  }

  def partI(vs : Array[Int], p : Int, r : Int) = {
    var i = p
    for (j <- (p + 1 to r)) {
      if (vs(j) < vs(p)) {
        i = i + 1
        swap(vs, j, i) 
      }
    }
    swap(vs, p, i)
    i 
  }

  def quicksortI(ar : Array[Int], k : Int) : Array[Int] = quicksortI(ar, 0, ar.length - 1, k)
  
  // take k highest elements...
  def quicksortI(ar : Array[Int], p : Int, q : Int, k : Int) : Array[Int] = {
    if (p < q) {
      val pivot = partI(ar, p, q)
      if (ar.size - pivot < k) {
        //quicksortI(ar, p, pivot - 1, k)
        quicksortI(ar, pivot + 1, q, k)
      }
    }
    ar
  }

  def swap[T](vs : Array[T], a : Int, b : Int) = { val t = vs(a); vs(a) = vs(b); vs(b) = t }

  // insersion sort, recursive using lists...
  def insert(x : Int, xs : List[Int]) : List[Int] = xs match { case Nil => x :: Nil; case (y :: ys) => if (x < y) x :: xs else y :: insert(x, ys) }

  def iSort(ls : List[Int]) : List[Int] = ls match { case Nil => Nil; case (x :: xs) =>  insert(x, iSort(xs)) }


  def main(args : Array[String]) {
 
    val ar = Array(5, 7, 3, 5, 2, 3, 1, 9, 1, 5, 2, 0)
    println("Array = " + ar.toList.mkString(", "))
    println("quicksortI = " + quicksortI(ar.clone).toList.mkString(", "))
    println("quicksortI - kth = " + quicksortI(ar, 5).toList.take(5).mkString(", "))
    println("quicksortI - kth = " + quicksortI(ar, 7).toList.take(7).mkString(", "))
  
    val ls = List(5, 3, 7, 6, 8, 9, 1, 4)
      
    println("insertion sort of " + ls.mkString(", ") + " = " + iSort(ls))
  }
}

object Trees {

  sealed trait TreeBase[+T]
  case object TreeNil extends TreeBase[Nothing]
  case class Tree[+T](value: T, left: TreeBase[T], right: TreeBase[T]) extends TreeBase[T]

  def inOrder[A](t: TreeBase[A], f: Tree[A] => Unit) : Unit = t match {
    case TreeNil =>
    case tree @ Tree(x, left, right) =>
      inOrder(left, f)
      f(tree)
      inOrder(right, f)
  }

  def main(args : Array[String]) = {

    val tree1 = Tree(1,
      Tree(2,
           Tree(4, TreeNil, TreeNil),
           TreeNil),
      Tree(3,
           Tree(5, TreeNil, TreeNil),
           Tree(6, TreeNil, TreeNil))
    )

    inOrder(tree1, (t : Tree[Int]) => println(t.value))
 
    println("-")
    inOrderIter(tree1)
  }

  // iterative
  def inOrderIter[T](t : TreeBase[T]) {
    val s = new java.util.Stack[TreeBase[T]]()
    var n = t
    while (!s.isEmpty() || n != TreeNil) {
      n match {
        case x @ Tree(_, l, _) => {
          s.push(x)
          n = l
        }
        case TreeNil => {
          n = s.pop()
          n match {
            case Tree(v, _, r) =>
              println(v)
              n = r
            case _ =>
	  }
        }
      }
    }
  }
}

object Peano {

  trait Num
  case object Zero extends Num
  case class Succ(v : Num) extends Num

  def main(args: Array[String]) {
	
    val a = (Succ(Succ(Zero)))
    val b = Succ(Succ(Succ(Zero)))
    val c = Succ(Succ(Succ(Succ(Zero))))
	
    println("a = " + value(a))
    println("b = " + value(b))
    println("c = " + value(c))
	
    println("Num 5 = " + num(5))
    println("value(num(5)) = " + value(num(5)))
	
    println("num(5) + num(3)) = " + value(add(num(5), num(3))))
  }
  
  def add(a : Num, b : Num) : Num = a match { case Zero => b; case Succ(p) => add(p, Succ(b)) }
  
  def value(v : Num) : Int = v match { case Zero => 0; case Succ(p) => 1 + value(p); }
  
  def num(v : Int) : Num = v match { case 0 => Zero; case n => Succ(num(n-1)) }
}

object Polynomials {
  
  case class Term(coefficient : Int, exponent : Int)

  case class Polynomial(terms : List[Term])

  def eval(p : Polynomial, x : Int) = 
    p.terms.foldLeft(0)((s : Int, t : Term) => s + (t.coefficient * pow(x, t.exponent).toInt))

  def findRoots(p : Polynomial, m : Int) = (0 to m).filter(x => eval(p, x) % m == 0)

  def main(args : Array[String]) = {
    
    val p1 = Polynomial(List(Term(1, 2), Term(4, 1), Term(-4, 0)))
    val p2 = Polynomial(List(Term(1, 3), Term(2, 2), Term(1,1), Term(3, 0)))

    val r1 = findRoots(p1, 7)
    val r2 = findRoots(p2, 7)

    println("results %d, %d", r1, r2)
  }
}

object Congruences {

  case class LinCongruence(a : Int, b : Int, mod : Int) {
    def eval(x : Int) = ((a * x) - b) % mod
  }
}

//Primes.main(args)
//Sorting.main(args)
//Polynomials.main(args)
M381.main(args)


