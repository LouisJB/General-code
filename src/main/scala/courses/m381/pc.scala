#!/bin/bash
exec scala "$0" "$@"
!#

import scala.util.parsing.combinator._
import scala.util.matching.Regex

object InstrParser extends RegexParsers {

// for { _ < char('('); ds <- rep(for { _ <- spaces; d <- digit } yield d); _ < char(')') } yield ds

  private def space = """\s+""".r
  private def number = regex(new Regex("[0-9]+[ \\n]+")) ^^ { s => s.toInt }
  private def z = regex(new Regex("[Z]")) ^^ { i => Zi }
  private def s = regex(new Regex("[S]")) ^^ { i => Si }
  private def c = regex(new Regex("[C]")) ^^ { i => Ci }
  private def j = regex(new Regex("[J]")) ^^ { i => Ji }

  //private def instrCode = regex(new Regex("[ZCJS]")) ^^ { i : String => { val c : Char = i.toCharArray()(0); Ii(c) } }
  private def instrCode = z | s | c | j
  private def lparen = regex(new Regex("\\("))
  private def rparen = regex(new Regex("\\)"))
  private def args = regex(new Regex("[0-9]+[,]*[ ]?"))
  private def arg2 = regex(new Regex("[0-9]+[,]*[ ]?")) ^^ { case x => x.toInt } 
  private def arg = regex(new Regex("[0-9]+")) ^^ { case x => x.toInt }
  private def arguments = "(" ~> repsep(arg, ",") <~ ")" ^^ { case x => x }

  private def instr = instrCode /* ~ lparen */ ~ arguments /* <~ rparen */ ^^ {
    case instrCode /* ~ lparen */ ~ arguments => I(instrCode, arguments) // ">" + instrCode + " " + args + "<"
  }

  def parseItem(str: String): ParseResult[Any] = parse(instr, str)

  def main(args : Array[String]) {

    InstrParser.parseItem(args(0)) match {
      case InstrParser.Success(result, _) => {
        println("Parsed as: " + result.toString)
        println("args: " + result)
      }
      case _ => println("Could not parse the input string.")
    }
  }
}

// URM machine instructions
abstract trait Ii
trait UDEFi extends Ii
case object Ni extends Ii
case object Zi extends Ii
case object Si extends Ii
case object Ci extends Ii
case object Ji extends Ii

object Ii {
  def apply(t : Char) = {
    
     t match {
      case 'N' => Ni
      case 'Z' => Zi
      case 'S' => Si
      case 'C' => Ci
      case 'J' => Ji
      case _ => throw new Exception("Unknown instruction during parsing: " + t)
    }
  }
}
 
abstract trait I
case object UDEF extends I
case object N extends I // nop
case class Z(n : Int) extends I // Zero
case class S(n : Int) extends I // successor
case class C(n : Int, m : Int) extends I // copy
case class J(n : Int, m : Int, q : Int) extends I // Jump

object I {
  def apply(s : String) : I = {

    // take the first char as the instruction ID symbol
    val instr = Ii(s(0))

    // first char is instr, the rest as the instruction params
    val argStr = s.drop(1)  
    val args = argStr.filter(c => c != '(' && c != ')').split(",", 0).map(_.trim.toInt).toList   

    apply(instr, args)
  }

  def apply(instr : Ii, args : List[Int]) : I = {

    // and the rest as the instruction params
    //val args = ar.filter(c => c != '(' && c != ')').split(",", 0).map(_.trim.toInt).toList      

    instr match {
      case Ni => N
      case Zi => Z(args(0))
      case Si => S(args(0))
      case Ci => C(args(0), args(1))
      case Ji => J(args(0), args(1), args(2))
      case _ => throw new Exception("Unknown instruction during parsing: " + instr)
    }
  }

  def apply(n : Int) = decode(n)

  def decode(n : Int) =
    if (n % 3 == 0 && n % 6 != 0 && (n+3) % 6 == 0) Z((n+3)/6)
    else if (n % 6 == 0) S(n/6)
    else if (n % 3 == 1 && n % 2 == 1) C(factor(n-1, 2), factor(n-1, 3))
    else if (n % 3 == 2 && n % 2 == 0 && n % 5 == 2) J(factor(n-2, 2), factor(n-2, 3), factor(n-2, 5))
    else UDEF

  def factor(n : Int, m : Int) : Int = if (n % m != 0) 0 else 1 + factor(n/m, m)
}


object FormulaParser extends RegexParsers {

  private def space = """\s+""".r

  //((x0 . x2) + (x2 + 0′))
  private def expr : Parser[Any] = term ~ rep("+" ~ term | "-" ~ term | "*" ~ term | "/" ~ term)
  private def term : Parser[Any] = """[0-9]+""".r | "(" ~ expr ~ ")"

  def parseItem(str: String): ParseResult[Any] = parse(expr, str)

  def main(args : Array[String]) {
    println("parsing " + (args(0)))
    FormulaParser.parseItem(args(0)) match {
      case FormulaParser.Success(result, _) => {
        println("Parsed as: " + result.toString)
        println("args: " + result)
      }
      case _ => println("Could not parse the input string.")
    }
  }
}

FormulaParser.main(args)
