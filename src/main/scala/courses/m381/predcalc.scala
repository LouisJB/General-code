package courses.m381

/*
#!/bin/sh
exec scala "$0" "$@"
!#
*/

import math._

trait  Expr {
  def apply(ex : Expr) = ex
}
trait BinExpr {
  def apply(ex1 : Expr, ex2 : Expr) = ex1
}
case class Number(value : Double) extends Expr
case class UnaryOp(operator : Expr, arg : Expr) extends Expr
case class BinaryOp(operator : BinExpr, left : Expr, right : Expr) extends Expr

trait Connectives extends Expr
case object Not extends Connectives {
  override def apply(ex : Expr) = {
    ex match {
      case True => False; case False => True
    }
  }
}

case object And extends Connectives with BinExpr {
  override def apply(ex1 : Expr, ex2 : Expr) = {
    ex1 match {
      case False => False
      case _ => ex2 match {
        case True => True
        case _ => False
      }
    }
  }
}

case object Or extends Connectives {
  def apply(ex1 : Expr, ex2 : Expr) = {
    ex1 match {
      case True => True
      case _ => ex2 match {
        case True => True
        case _ => False
      }
    }
  }
}

case object Impl extends Connectives {
  def apply(ex1 : Expr, ex2 : Expr) = {
    ex1 match {
      case False => True
      case _ => ex2 match {
        case True => True
        case _ => False
      }
    }
  }
}

case object Biimpl extends Connectives

trait Quantifiers extends Expr
case object ForAll extends Quantifiers
case object Exists extends Quantifiers

trait Value extends Expr
case object True extends Value
case object False extends Value
case object Zero extends Value

trait Symbol extends Expr
case object Prime extends Symbol
case object Plus extends Symbol
case object dot extends Symbol

object PredCalc {

  import PredList._
  import RichInt._
  import RichBoolean._

  def main(args : Array[String]) {

    val ex1 = eval(UnaryOp(Not, True))
    println("value = " + ex1)
 
    val ex2 = eval(UnaryOp(Not, UnaryOp(Not, True)))
    println("value = " + ex2)

    val ex3 = eval(BinaryOp(And, True, False))
    println("value = " + ex3)
  }

  def eval(ex : Expr) : Expr = {

    ex match {
      case UnaryOp(op, arg) => op(eval(arg))
      case BinaryOp(op, arg1, arg2) => op(eval(arg1), eval(arg2))
      case True => True
      case False => False
      case _ => ex
    }
  }

  def testRichObjects {
    val p = (e : Int) => e % 2 == 0
    val r = List(2, 4, 6, 7) ∀ p

    println("result = " + r )

    val r2 = 6 ∣ 18
    println("result = " + r2)

    val r3 = true ∨ false
    println(r3)

    println(4++)
    println(5!)
    println(4 x 6)
    println(6 c 4)
    println(6 p 3)

    println("8 ≋ 14 (mod 6): " + 8 ≋(14, 6))
    println("8 ≋ 14 (mod 7): " + 8 ≋(14, 7))
  }
}

case class PredList[T](ls : List[T]) {
  def ∀(p : T => Boolean) : Boolean = ls.forall(p)
}

object PredList {
  implicit def toPredList[T](ls : List[T]) : PredList[T] = PredList(ls)
}

case class RichInt(n : Int) {
  def ∣(x : Int) : Boolean = x % n == 0 // divides
  def ∤(x : Int) : Boolean = x % n != 0 // not divide
  def x(a : Int) : Int = a * n
  def ++ : Int = n + 1
  def `'` : Int = n + 1
  def ! : Int = (2 to n).foldLeft(1)((a : Int, b : Int) => a * b)

  def ≋(a : Int, m : Int) = a % m == n % m
 
  def p(r : Int) : Int = (RichInt(n)!) / (RichInt(n - r)!)
  def c(r : Int) : Int = p(r) / (RichInt(r)!) // (RichInt(n - r)!) / (RichInt(RichInt(n - r)!) x (RichInt(r)!))
}

object RichInt {
  implicit def toRichInt(n : Int) : RichInt = RichInt(n)
}

case class RichBoolean(b : Boolean) {
  def ∨(e : Boolean) : Boolean = b || e
  def ¬ : Boolean = !b
}

object RichBoolean {
  implicit def toRichBoolean(b : Boolean) : RichBoolean = RichBoolean(b)
}

//PredCalc.main(args)
