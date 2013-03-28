/**
 * Scala 2.8+ Maze Applet v0.1 20-09-2009 Louis Botterill
 */
package maze

import scala.swing.Applet
import scala.swing.Panel
import scala.swing.Reactor

import javax.swing._
import java.awt.Graphics
import java.awt.Color


object Direction extends Enumeration {
  type Direction = Value
    val North, South, East, West = Value
}

object Breadcrumb extends Enumeration {
  type Breadcrumb = Value
    val Forward, Backward, Clear = Value
}

import Direction._
import Breadcrumb._

class MazeModel {
  val HEIGHT = 50
  val WIDTH = 50

  // instantiate and initialise the 2d array - there is a direct 2d version but not working currently in scala 2.8.n
  var cells = Array.tabulate(WIDTH, HEIGHT)((i, j) => Cell(i, j))

  // set everything as not visited and with no trail
  def clearVisited() =
    cells.foreach(_.foreach(c => {
          c.visited = false;
          c.trail = Clear
      }))

  // generate maze
  def generateMaze(update :  => Unit) {

    val exit = new Point(WIDTH - 1, HEIGHT - 1)
    val start = new Point(0, 0)

    // 1. Start at a particular cell, call it the "exit"
    // 2. Mark the current cell as visited, and get a list of all its neighbors.
    //      For each neighbor, starting with a randomly selected neighbor:
    //   1. If that neighbor hasn't been visited,
    //        remove the wall between this cell and that neighbor,
    //        and then recurse with that neighbor as the current cell.

    // cell 0,0 is the start, open the north wall to highlight this
    var s = cells(0)(0)
    s.clear(North)

    // cell width-1,height-1 is the exit, open the south wall to highlight this
    val c = cells(exit.x)(exit.y)
    c.clear(South)

    // recursively process the next cell
    def doNextCell(c : Cell) {

      c.visited = true
      c.trail = Forward

      val dirs = c.getRndDirections()

      dirs.foreach(dir => {
        val x = getCell(c, dir)

        x match {
          case Some(n) =>
            
            if (n.visited != true) {
              n.visited = true
              c.clear(dir)
              n.clear(getInv(dir))
              n.trail = Forward
              update
              doNextCell(n)
              n.trail = Clear
              update
            }

          case None =>
        }
      })
    }

    doNextCell(c)
  }

  def solveMaze(update :  => Unit) {
    solveMazeBFS(update)
  }

  // find the maze solution using dfs from the start node until the end
  // node is located
  def solveMazeDFS(update : => Unit) {
    
    val start = new Point(0, 0)
    val exit = new Point(WIDTH - 1, HEIGHT - 1)

    val s = cells(0)(0)
    s.clear(North);

    def doNextCell(c : Cell) {

      c.visited = true
      c.trail = Forward

      val dirs = c.getDirections()
      
      dirs.foreach(dir => {
        
	    val x = getCell(c, dir)

        x match {
          case Some(n) =>

            if (n.visited != true) {
              n.visited = true
              n.pi = c  // set predecessor node
              n.trail = Forward
              update
              if (n.i == exit.x && n.j == exit.y) throw new Exception("Done")
              doNextCell(n)
              n.trail = Backward
              update
            }

          case _ =>
        }

        Thread.sleep(1) // add a little delay so we can watch the dfs explore and find the solution
      })
    }

    doNextCell(s)
  }

  // find the maze solution using dfs from the start node until the end
  // node is located
  def solveMazeBFS(update :  => Unit) {

    val q = new scala.collection.mutable.Queue[Cell]

    val start = new Point(0, 0)
    val exit = new Point(WIDTH - 1, HEIGHT - 1)

    val s = cells(0)(0)
    s.clear(North);

    q.enqueue(s)
    var gen = 0

    while (!q.isEmpty) {

      val c = q.dequeue
      c.gen = q.size
      if (c.visited != true) {

        c.visited = true
        c.trail = Forward

        update

        if (c.i == exit.x && c.j == exit.y) throw new Exception("Done")

        val dirs = c.getDirections()

        dirs.foreach(dir => {

          val x = getCell(c, dir)

          x match {
            case Some(n) =>
              if (n.visited != true) {
                n.pi = c
                n.gen = c.gen
                q.enqueue(n)
                gen = gen + 1
              }
            case _ =>
          }
        })

        Thread.sleep(5) // add a little delay so we can watch the dfs explore and find the solution
      }
    }
  }

  // from the exit recurse over the trail of predecessors,
  // marking with a forward trail
  def showSolution(update :  => Unit) {

    val exit = new Point(WIDTH - 1, HEIGHT - 1)

    println(">>showSolution")

    // 2.8 avoid var using Stream
    // Stream.iterate(cells(exit.n)(exit.y))(_.pi).takeWhile(_ != null)
    Stream.iterate(cells(exit.x)(exit.y))(_.pi).takeWhile(_ != null).foreach{
      n => {
        n.trail = Forward
        update
      }
    }

    println("<<showSolution")
  }

  // get the cell if possible based on current cell and the given direction
  def getCell(c : Cell, dir : Direction) : Option[Cell] = dir match {

    case North  => if (c.j > 0) Some(cells(c.i)(c.j-1)) else None
    case South  => if (c.j < HEIGHT  - 1) Some(cells(c.i)(c.j+1)) else None
    case East   => if (c.i < WIDTH - 1) Some(cells(c.i+1)(c.j)) else None
    case West   => if (c.i > 0) Some(cells(c.i-1)(c.j)) else None
  }

  // find the inverse direction for a given direction
  def getInv(dir : Direction) = dir match {

    case North => Direction.South
    case South => Direction.North
    case East => Direction.West
    case West => Direction.East
  }
}

// a scala.swing.Panel, override paint(Graphics2D) to paint each of the cells
class MazePanel(m : MazeModel) extends Panel {

  override def paint(g : java.awt.Graphics2D) =
    m.cells.foreach(_.foreach(_.draw(g)))
}

// a scala.swing.Applet based applet using scala swing wrappers
class MazeApplet extends Applet {

  val m = new MazeModel()
  lazy val mp = new MazePanel(m)

  object ui extends UI with Reactor {

    def init() = {
      contents = mp
    }

    override def start() = {
      def update = {
        this.repaint()
      }

      m.generateMaze(update)

      Thread.sleep(2000)

      m.clearVisited

      try {
        m.solveMazeDFS(update);
      }
      catch {
        case e : Exception =>
      }

      Thread.sleep(2000)

      m.clearVisited
      m.showSolution(update)
      mp.repaint()

      Thread.sleep(2000)

      m.clearVisited

      try {
        m.solveMazeBFS(update);
      }
      catch {
        case e : Exception =>
      }

      Thread.sleep(2000)

      m.clearVisited
      m.showSolution(update)
      mp.repaint()

      Thread.sleep(2000)

      m.clearVisited

      this.repaint()

      Thread.sleep(2000)

    }
  }
}

// standard Java app of the maze using JFrame
object Maze {

  def main(args: Array[String]) {

    val mazeApplet = new MazeApplet()
    val frame = new JFrame()
    frame.setBounds(50, 50, 750, 750)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    frame.add(mazeApplet)

    frame.setVisible(true)
    mazeApplet.init
    mazeApplet.start
  }
}

// represent a Cell of the maze
case class Cell(i: Int, j : Int) {
  
  val size = 7; // each cell is 7px by 7px
  var north : Boolean = true
  var south : Boolean = true
  var east : Boolean = true
  var west : Boolean = true
  var visited : Boolean = false
  var pi : Cell = null  // predecessor cell
  var trail : Breadcrumb = Clear
  var gen : Int = 0

  def draw(g : Graphics) {

    val x = i * size
    val y = j * size

    trail match {
      case Forward => {
        val c = getColour(gen)
        g.setColor(c)
        g.fillRect(x + 2, y + 2, size - 4, size - 4)
      }
      case Backward => {
        g.setColor(Color.BLUE)
        g.fillRect(x + 2, y + 2, size - 4, size - 4)
      }
      case _ =>
    }

    g.setColor(Color.BLACK)

    if (north) g.drawLine(x, y, x + size, y)
    if (south) g.drawLine(x, y + size, x + size, y + size)
    if (east) g.drawLine(x + size, y, x + size, y + size)
    if (west) g.drawLine(x, y, x, y + size)
  }

  def clear(dir : Direction) = dir match {

    case North => north = false
    case South => south = false
    case East => east = false
    case West => west = false
  }

  def getDirections() : scala.List[Direction] = {

    var d = scala.List[Direction]()

    if (!north) d = North :: d
    if (!south) d = South :: d
    if (!east) d = East :: d
    if (!west) d = West :: d

    d
  }

  def getRndDirections() : scala.List[Direction] = {

    val directions = North :: South :: East :: West :: Nil
    implicit val r = Rand.rand
    Utils.shuffle(directions) //, Rand.rand)
  }

  def getColour(n : Int) = n match {
    case 0 => Color.RED
    case 1 => Color.BLUE
    case 2 => Color.CYAN
    case 3 => Color.YELLOW
    case 4 => Color.PINK
    case 5 => Color.ORANGE
    case 6 => Color.MAGENTA
    case _ => Color.GREEN
  }
}

// represent a 2d point
case class Point(x : Int, y : Int)

// init a random generator singleton
object Rand {
  val rand : scala.util.Random = new scala.util.Random();
}

object Utils {
  
  // ref: http://okmij.org/ftp/Haskell/perfect-shuffle.txt
  // scala 2.7+ here's an O(n log n) solution:
  /*
  def shuffle[T](xs: List[T], r: scala.util.Random) = {
    xs.toStream.zip(Stream.const(r.nextDouble _).map(_())).toList.sort(_._2 < _._2).map(_._1)
  }
  */
  
  // scala 2.8+
  def shuffle[T](xs: List[T])(implicit r: scala.util.Random) = {
    xs.zip(Stream.continually(r.nextDouble)).sortWith(_._2 < _._2).map(_._1)
  }

  def permute[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => scala.List(Nil)
    case _ => xs.flatMap(x => permute(xs.filter(_ != x)).map(x :: _))
  }

  // make a stream of n, f(n), f(f(n)), etc.
  // in Haskell this is called "iterate".  it ought to be in the standard library
  // as Stream.iterate.  "unfold" should be more general, but nonetheless I'm
  // going to call this unfold for the moment...
  def unfold[T](x:T)(f:T=>T):Stream[T] =
    Stream.cons(x,unfold(f(x))(f))

  def iterate[T](x:T)(f:T=>T):Stream[T] =
    Stream.cons(x,iterate(f(x))(f))
}
