package courses.m381

/*
#!/bin/sh
exec scala "$0" "$@"
!#
*/

import scala.io.Source._
import java.io.File
import math._
import scala.annotation.tailrec

object URM {

  type Register = Int

  // program arguments
  // example run programs/foobar.urm data "1 1, 2 3"
  val DECODE_CMD = "decode"
  val RUN_CMD = "run"
  val DATA_CMD = "data"
  val CONCAT_CMD = "andThen"
  val keywords = List("data", "def")

  case class Program(name : String, instructions : List[I], registers : List[Register]) {
    println("program name = " + name)
    println("instructions = " + toIndexedList(instructions).mkString(", "))
    println("registers = " + toIndexedList(registers).mkString(", "))

    def toIndexedList[T](ls : List[T]) = ls.zip(1 to ls.size).map(x => "" + x._2 + " " + x._1)
  }

  // URM machine instructions
  abstract trait I
  case object UDEF extends I
  case object N extends I // nop
  case class Z(n : Int) extends I // Zero
  case class S(n : Int) extends I // successor
  case class C(n : Int, m : Int) extends I // copy
  case class J(n : Int, m : Int, q : Int) extends I // Jump

  object I {
    def apply(s : String) = {

      // take the first char as the instruction ID symbol
      val t = s(0)
      
      // and the rest as the instruction params
      val args = s.drop(1).filter(c => c != '(' && c != ')').split(",", 0).map(_.trim.toInt).toList      

      t match {
        case 'N' => N
        case 'Z' => Z(args(0))
        case 'S' => S(args(0))
        case 'C' => C(args(0), args(1))
        case 'J' => J(args(0), args(1), args(2))
        case _ => throw new Exception("Unknown instruction during parsing: " + t)
      }
    }

    def apply(n : Int) = decode(n)

    def decode(n : Int) =
      if (n % 3 == 0 && n % 6 != 0 && (n+3) % 6 == 0) Z((n+3)/6)
      else if (n % 6 == 0) S(n/6)
      else if (n % 3 == 1 && n % 2 == 1) C(factor(n-1, 2), factor(n-1, 3))
      else if (n % 3 == 2 && n % 2 == 0 && n % 5 == 2) J(factor(n-2, 2), factor(n-2, 3), factor(n-2, 5))
      else UDEF
  }
  
  def factor(n : Int, m : Int) : Int = if (n % m != 0) 0 else 1 + factor(n/m, m)

  val instructions = Array.tabulate[I](100)(n => UDEF)
  val registers = Array.tabulate[Register](100)(n => 0)
  val rTrace = true
  val iTrace = true

  def loadProgram(programFile : String, dataStr : Option[String] = None) = {

    val file = new File(programFile)
    println("Loading file from path: " + file.getAbsolutePath())

    val lines = fromFile(file).getLines.toList

    //lines.foreach(println)
    println("Loaded file %s, read %d lines, building program...".format(programFile, lines.size))

    val fileLines = lines.filter(l => !l.startsWith("#") && !l.startsWith("}") && !(l.trim.size < 1))

    val (progLines, dataLines) = fileLines.span(l => !l.startsWith("data"))

    val progName = progLines.find(l => l.startsWith("def")).getOrElse("").split(" ").toList.tail.head
    val program = progLines.filter(l => keywords.forall(k => !l.startsWith(k))).map(_.takeWhile(_ != '#'))

    val prog = program.map(p => instructionFromString(p)).toList
   
    //println("data = " + dataStr)
    
    val data = dataStr match {
      case None => dataLines.tail.map(d => dataFromString(d)).toList
      case Some(s) => s.split(",").map(_.trim).filter(_.size > 0).map(d => dataFromString(d)).toList
    }

    println("program loaded, using data set %s".format(data.mkString(", ")))
    Program(progName, prog, data)
  }

  def instructionFromString(instructionLine : String) = {
    val (number, instr) = instructionLine.span(c => c != ' ')
    I(instr.trim)
  }
  
  def dataFromString(dataLine : String) : Register = {
    val (number, data) = dataLine.span(c => c != ' ')
    data.trim.toInt
  }

  def concat(prog1 : Program, prog2 : Program) : Program = {

    val p1Size = prog1.instructions.size
    val p2Size = prog2.instructions.size

    val maxReg1 = prog1.instructions.map(i => i match { case S(x) => x; case C(a, b) => math.max(a, b); case _ => 0 }).max

    println("Max reg location = " + maxReg1)

    val prog1Relocated = prog1.instructions.map(i => i match {
      case J(x, y, z) if (z >= p1Size)  => J(x, y, p1Size + 1) // relocate all jump instructions
      case z => z
    })
   
    val zeroInstrs = (2 to maxReg1).map(x => Z(x)).toList

    val prog2Relocated = prog2.instructions.map(i => i match {
      case J(x, y, z) => J(x, y, z + p1Size + maxReg1 - 1) // relocate all jump instructions
      case z => z
    })

    val concatPrg = prog1Relocated ::: zeroInstrs  ::: prog2Relocated

    Program("Concat " + prog1.name + "_" + prog2.name, concatPrg, prog1.registers)
  }

  def main(args : Array[String]) {

    args.foreach(println)

    if (args.size >= 2) {
      if (args(0) == RUN_CMD) {
        println("loading: " + args(1))
        val program = if (args.size >= 4 && args(2) == DATA_CMD) {
          val dataStr = args(3)
          loadProgram(args(1), Some(dataStr))
        }
        else
          loadProgram(args(1))

        loadAndRun(program)
      }
      else if (args(1) == CONCAT_CMD) {
        val prog1 = loadProgram(args(0))
        val prog2 = loadProgram(args(2))
        val concatProg = concat(prog1, prog2)
        println("Concatentated as: '" + concatProg.name + "'")
        println(concatProg.toIndexedList(concatProg.instructions).mkString("\n"))

        loadAndRun(concatProg.name, concatProg.instructions, concatProg.registers)
      }
      else if (args(0) == DECODE_CMD) {
        println("Decoded to: " + I(args(1).toInt))
      }
      else {
        println("Unknown command " + args(0))
      }
    }
    else {
      println("No command!")
    }
  }

  def clear() = for(i <- 0 to registers.length - 1) registers(i) = 0

  def loadAndRun(p : Program) : Register  = loadAndRun(p.name, p.instructions, p.registers)

  def loadAndRun(name : String, instrs : List[I], mem : List[Register]) = {

    clear()

    instructions.forall(_ == 0)

    arrayAddList(instructions, instrs)
    arrayAddList(registers, mem)
    
    println("Initialised registers: " + regToStr(registers))
    println("Initialised instructions: " + iToStr(instructions))
    println("Running program '%s' \n".format(name))
      
    val result = run

    println("Program terminated with result: " + result)
    
    result
  }

  def run : Int = run(0)

  @tailrec
  def run(pcc : Int) : Register = {
 
    var pc = pcc
    val i = instructions(pc)
   
    if (iTrace) println("Running instruction %s at %d".format(i.toString, pc + 1))
    if (rTrace) println("Registers: %s".format(regToStr(registers)))

    i match {
      case UDEF => return registers(0)
      case N => // nop
      case Z(n) => registers(n - 1) = 0
      case S(n) => registers(n - 1) = registers(n - 1) + 1
      case C(n, m) => registers(m - 1) = registers(n - 1)
      case J(n, m, q) => if (registers(n - 1) == registers(m - 1)) pc = q - 2
    }

    run(pc + 1)
  }

  def arrayAddList[T](ar : Array[T], ls : List[T]) = for (i <- 0 to ls.length-1) ar(i) = ls(i)
  def regToStr(ar : Array[Register]) = 
    ar.zip(1 to ar.size).takeWhile(_._1 != 0).map(rt => rt._2 + " = " + rt._1).mkString(", ") 

  def iToStr(ar : Array[I]) = ar.takeWhile(_ != UDEF).mkString(", ") 
}

//URM.main(args)

