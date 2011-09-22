package excercises

import org.scalacheck._

// http://blog.tmorris.net/revised-scala-exercises/
//

sealed trait List[+A] {
  override def toString = {
    def toScalaList(t: List[A]): scala.List[A] = t match {
      case Empty => Nil
      case Cons(h, t) => h :: toScalaList(t)
    }
    toScalaList(this).toString
  }
}

final case object Empty extends List[Nothing]
final case class Cons[A](h: A, t: List[A]) extends List[A]

object List {
  def foldRight[A, B](as: List[A], b: B, f: (A, B) => B): B = as match {
    case Empty => b
    case Cons(h, t) => f(h, foldRight(t, b, f))
  }

  def foldLeft[A, B](as: List[A], b: B, f: (B, A) => B): B = as match {
    case Empty => b
    case Cons(h, t) => foldLeft(t, f(b, h), f)
  }

  def reduceRight[A](as: List[A], f: (A, A) => A): A = as match {
    case Empty => error("bzzt. reduceRight on empty list")
    case Cons(h, t) => foldRight(t, h, f)
  }

  def reduceLeft[A](as: List[A], f: (A, A) => A): A = as match {
    case Empty => error("bzzt. reduceLeft on empty list")
    case Cons(h, t) => foldLeft(t, h, f)
  }

  def unfold[A, B](b: B, f: B => Option[(A, B)]): List[A] = f(b) match {
    case Some((a, b)) => Cons(a, unfold(b, f))
    case scala.None => Empty
  }
}

sealed trait Natural {
  override def toString = {
    def toInt(n: Natural): Int = n match {
      case Zero => 0
      case Succ(x) => 1 + toInt(x)
    }
    toInt(this).toString
  }
}

final case object Zero extends Natural
final case class Succ(c: Natural) extends Natural


object Exercises {
  val One = Succ(Zero)
  val Two = Succ(One)
  val Three = Succ(Two)

  def toNatural(n : Int) : Natural =  n match {
    case 0 => Zero
    case _ => Succ(toNatural(n-1))
  }

  def main(args: Array[String]){

    val ls = Cons(1, Cons(2, Cons(3, Cons(4, Empty))))
    val ys = Cons(11, Cons(12, Cons(13, Cons(14, Empty))))
    val zs = Cons(22, Cons(23, Cons(24, Empty)))

    println("add      : " + add(Zero, Two))
    println("add      : " + add(One, Zero))
    println("add      : " + add(One, Two))
    println("add      : " + add(Two, toNatural(5)))

    println("sum      : " + sum(ls))

    println("length   : " + length(ls))

    println("map      : " + map(ls, (x: Int) => x * 2 ))

    println("filter   : " + filter(ls, (x: Int) => x > 2))

    println("append   : " + append(ls, ys))

    println("flatten  : " + flatten(Cons(ls, Cons(ys, Cons(zs, Empty)))))

    println("flatMap  : " + flatMap(ls, (x: Int) => Cons(x, Cons(x, Empty)) ))

    println("maximum  : " + maximum(ls))

    println("reverse  : " + reverse(ls))

    println("identity : " + identity(ls))

    println("interperse : " + intersperse(ls, 0))

    println("intercalate : " + intercalate(ls, ys))
  }

  // Exercise 1
  // Relative Difficulty: 1
  // Correctness: 2.0 marks
  // Performance: 0.5 mark
  // Elegance: 0.5 marks
  // Total: 3
  def add(x: Natural, y: Natural): Natural = x match {
    case Zero => y
    case Succ(z) => add(z, (y))
    //case Succ(z) => Succ(add(z, y)) // or this
  }

  // Exercise 2
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def sum(is: List[Int]): Int =
    List.reduceLeft(is, (x: Int, y: Int) => (x + y))
    //List.foldLeft(is, 0, (x: Int, y: Int) => (x + y))

  // Exercise 3
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def length[A](as: List[A]): Int =
    List.foldLeft(as, 0, (a: Int, _: A) => a+1)

  // Exercise 4
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.0 mark
  // Elegance: 1.5 marks
  // Total: 7
  def map[A, B](as: List[A], f: A => B): List[B] =
    List.foldRight(as, Empty, (x: A, ys: List[B]) => Cons(f(x), ys))

  // Exercise 5
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def filter[A](as: List[A], f: A => Boolean): List[A] =
    List.foldRight(as, Empty, (x: A, ys: List[A]) => if (f(x) == true) Cons(x, ys) else ys)

  // Exercise 6
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def append[A](x: List[A], y: List[A]): List[A] =
    List.foldRight(x, y, (x : A, ys: List[A]) => Cons(x, ys))

  // Exercise 7
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def flatten[A](as: List[List[A]]): List[A] =
    List.foldRight(as, Empty, (xs: List[A], ys: List[A]) => append(xs, ys))

  // Exercise 8
  // Relative Difficulty: 7
  // Correctness: 5.0 marks
  // Performance: 1.5 marks
  // Elegance: 1.5 mark
  // Total: 8
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    List.foldRight(as, Empty, (x: A, ys: List[B]) => append(f(x), ys))

  // Exercise 9
  // Relative Difficulty: 8
  // Correctness: 3.5 marks
  // Performance: 3.0 marks
  // Elegance: 2.5 marks
  // Total: 9
  def maximum(is: List[Int]): Int =
    List.reduceLeft(is, (x: Int, y: Int) => if (x > y) x else y)

  // Exercise 10
  // Relative Difficulty: 10
  // Correctness: 5.0 marks
  // Performance: 2.5 marks
  // Elegance: 2.5 marks
  // Total: 10
  def reverse[A](as: List[A]): List[A] =
    List.foldLeft(as, Empty, (ys: List[A], x: A) => Cons(x, ys))

  def identity[A](ls : List[A]): List[A] = {
    List.foldRight(ls, Empty, (x: A, xs : List[A]) => Cons(x, xs))
  }

  def intersperse[A](ls : List[A], a: A): List[A] = {
    flatMap(ls, (x: A) => Cons(x, Cons(a, Empty)))
  }

  def intercalate[A](ls : List[A], as : List[A]) : List[A] = {
    flatMap(ls, (x: A) => Cons(x, as))
  }
}

object ListSpecification extends Properties("List") {
  import Prop.forAll

  import Exercises._

  property("sum") = forAll { (ls: scala.List[Int]) =>
    (0 /: ls)(_+_) == sum(toList(ls))
  }

  property("length") = forAll { (ls: scala.List[Int]) =>
    ls.length == length(ls)
  }

  property("map") = forAll { (ls: scala.List[Int], n : Int) =>
    ls.map(_ * n) == Exercises.map(ls, (x : Int) => x * n)
  }

  property("filter") = forAll { (ls: scala.List[Int]) =>
    ls.filter(_ % 2 == 0) == Exercises.filter(ls, (x : Int) => x % 2 == 0)
  }

  property("append") = forAll { (xs: scala.List[Int], ys: scala.List[Int]) =>
    xs ::: ys == Exercises.append(xs, ys)
  }

  property("flatten") = forAll { (xs: scala.List[scala.List[Int]]) =>
    xs.flatten == Exercises.flatten(xs.map(toList(_)))
  }

  property("flatmap") = forAll { (xs: scala.List[Int]) =>
    xs.flatMap(x => scala.List(x, x)) ==
            Exercises.flatMap(toList(xs), (x : Int) => Cons(x, Cons(x, Empty)))
  }

  property("maximum") = forAll { (xs: scala.List[Int]) =>
    if (xs.length > 0)
      xs.max == Exercises.maximum(xs)
    else
      true
  }

  property("reverse") = forAll { (xs: scala.List[Int]) =>
     xs.reverse == Exercises.reverse(xs)
  }

  property("identity") = forAll { (xs: scala.List[Int]) =>
     xs == Exercises.identity(xs)
  }

  implicit def toList[T](ls : scala.List[T]) : List[T] = ls match {
    case Nil => Empty
    case (x :: xs) => Cons(x, toList(xs))
  }
}
