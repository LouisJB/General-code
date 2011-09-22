package maze

import scala.swing.Applet
import scala.swing.Panel
import scala.swing.Reactor

import javax.swing._;
import java.awt.Graphics;
import java.awt.Color;


object Direction2 extends Enumeration {
  type Direction = Value
    val North, South, East, West = Value
}

// alternatively
//abstract sealed class Direction;
//case object North extends Direction;
//case object South extends Direction;
//case object East extends Direction;
//case object West extends Direction;

object Breadcrumb2 extends Enumeration {
  type Breadcrumb2 = Value
    val Forward, Backward, Clear = Value
}

import Direction._
import Breadcrumb._

class MazeModel2 {
  val HEIGHT = 100
  val WIDTH = 100

  /*
   * this is simple array of array code compatible with 2.7 but not currently in 2.8 due to bugs
  var cells : Array[Array[Cell]] = new Array[Array[Cell]](WIDTH, HEIGHT)

  // instantiate/initialise the Cell objects
  for (i <- 0 to cells.length - 1) {
      cells(i) = new Array[Cell](WIDTH);
      for (j <- 0 to cells(i).length - 1) {
        cells(i)(j) = new Cell(i, j)
      }
  }
  */

  // instead could use this for 2.8 (there is a direct 2d version but not working currently)
  var cells = Array.tabulate(WIDTH)(i => Array.tabulate(HEIGHT)(j => Cell(i, j)))

  // set everything as not visited and with no trail
  def clearVisited() = {
    /*
    for (i <- 0 to cells.length - 1) {
        for (j <- 0 to cells(i).length - 1) {
          cells(i)(j).v = false
          cells(i)(j).trail = None
        }
    }
    */
   
    cells.foreach(_.foreach((c) => {
          c.visited = false;
          c.trail = Clear
      }))
  }

  // generate maze
  def generateMaze(update :  => Unit) {

    val exit = new Point(WIDTH - 1, HEIGHT - 1)
    val start = new Point(0, 0)

    // From wikipedia description: http://en.wikipedia.org/wiki/Maze_generation_algorithm
    // 1. Start at a particular cell and call it the "exit."
    // 2. Mark the current cell as visited, and get a list of its neighbors.
    //      For each neighbor, starting with a randomly selected neighbor:
    //   1. If that neighbor hasn't been visited,
    //        remove the wall between this cell and that neighbor, and then recurse with that neighbor as the current cell.

    // cell 0,0 is the start, open the north wall to highlight this
    var s = cells(0)(0)
    s.clear(North);

    // cell width-1,height-1 is the exit, open the south wall to highlight this
    val c = cells(exit.x)(exit.y)
    c.clear(South);

    // recursively process the next cell
    def doNextCell(c : Cell) {
      c.visited = true
      c.trail = Forward

      var dirs = c.getRndDirections()

      while (!(dirs isEmpty)) {
        val dir = dirs.head
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

        dirs = dirs.tail

        //Thread.sleep(1)
      }
    }

    doNextCell(c)
  }

  // find the maze solution using dfs from the start node until the end
  // node is located
  def solveMaze(update :  => Unit) {
    val start = new Point(0, 0)
    val exit = new Point(WIDTH - 1, HEIGHT - 1)

    val s = cells(0)(0)
    s.clear(North);

    def doNextCell(c : Cell) {
      c.visited = true
      c.trail = Forward

      var dirs = c.getDirections()
      
      while (!(dirs isEmpty)) {
        val dir = dirs.head
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

        // take the top item off and use the rest of the list
        dirs = dirs.tail
        
        Thread.sleep(1)
      }
    }

    doNextCell(s)
  }

  // from the exit recurse over the trail of predecessors,
  // marking with a forward trail
  def showSolution(update :  => Unit) {
    val exit = new Point(WIDTH - 1, HEIGHT - 1)

    // 2.8+ avoid var using Stream
    // Stream.iterate(cells(exit.x)(exit.y))(_.pi).takeWhile(_ != null)
    Utils.iterate(cells(exit.x)(exit.y))(_.pi).takeWhile(_ != null).foreach{
      n => {
        n.trail = Forward
        update
      }
    }

    // working fine but replaced with the above to avoid unnecessary var
    /*
    var n = cells(exit.x)(exit.y)

    while (n != null) {

      n.trail = Forward
      n = n.pi
      update
    }
    */
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
class MazePanel_1(m : MazeModel) extends Panel {

  override def paint(g : java.awt.Graphics2D) {

    m.cells.foreach(_.foreach(_.draw(g)))
  }
}

// a javax.swing.JPanel, override paint(Graphics) to paint each of the cells
class MazePanel_2(m : MazeModel) extends JPanel {

  override def paint(g : Graphics) {

    m.cells.foreach(_.foreach(_.draw(g)))
  }
}

// a scala.swing.Applet based applet using scala swing wrappers
class MazeApplet_1 extends Applet {
  val m = new MazeModel()
  lazy val mp = new MazePanel_1(m)

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
        m.solveMaze(update);
      }
      catch {
        case e : Exception =>
      }

      Thread.sleep(2000)

      m.clearVisited

      m.showSolution(update)

      mp.repaint()
      this.repaint()
    }
  }
}

// standard javax.swing.JApplet of the maze
class MazeApplet_2 extends JApplet {

  val m = new MazeModel()
  lazy val mp = new MazePanel_2(m)

  def MazeApplet2 = {
  }

  override def init() {

    this.add(mp)
  }
  
  override def start() {
    def update = {
      this.repaint()
    }
    
    m.generateMaze(update)

    Thread.sleep(2000)

    m.clearVisited

    try {
      m.solveMaze(update);
    }
    catch {
      case e : Exception =>
    }

    Thread.sleep(2000)

    m.clearVisited

    m.showSolution(update)

    mp.repaint()
    this.invalidate()
    this.repaint()
  }
  
  override def stop() {
  }
  
  override def destroy() {
  }
}

// standard Java app of the maze using JFrame
object Maze2 {

  def main(args: Array[String]) {

    val mazeApplet = new MazeApplet()
    val frame = new JFrame()
    frame.setBounds(50, 50, 800, 800)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    frame.add(mazeApplet)

    frame.setVisible(true)
    mazeApplet.init
    mazeApplet.start
  }
}

// represent a Cell of the maze
case class Cell2(i: Int, j : Int) {
  
  val size = 7;
  var north : Boolean = true
  var south : Boolean = true
  var east : Boolean = true
  var west : Boolean = true
  var visited : Boolean = false
  var pi : Cell = null
  var trail : Breadcrumb = Clear;

  def draw(g : Graphics) {

    val x = i * size
    val y = j * size

    trail match {
      case Forward => {
        g.setColor(Color.RED);
        g.fillRect(x + 2, y + 2, size - 4, size - 4)
      }
      case Backward => {
        g.setColor(Color.BLUE);
        g.fillRect(x + 2, y + 2, size - 4, size - 4)
      }
      case _ =>
    }

    g.setColor(Color.BLACK);
    if (north) {
      g.drawLine(x, y, x + size, y);
    }
    if (south) {
      g.drawLine(x, y + size, x + size, y + size);
    }
    if (east) {
      g.drawLine(x + size, y, x + size, y + size);
    }
    if (west) {
      g.drawLine(x, y, x, y + size);
    }
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

    Utils2.shuffle(directions, Rand.rand)
  }

  def getRndDirections2() : scala.List[Direction] = {

    //val rand : scala.util.Random = new scala.util.Random();
    val d = Rand.rand.nextInt(3)

    d match {
      case 0 => North :: South :: East :: West :: Nil
      case 1 => East :: South :: North :: West :: Nil
      case 2 => South :: West :: North :: East :: Nil
      case 3 => West :: North :: East :: South :: Nil
    }
  }
}

// represent a 2d point
case class Point2(x : Int, y : Int)

// init a random generator singleton
object Rand2 {
  val rand : scala.util.Random = new scala.util.Random();
}

object Utils2 {
  //
  //http://okmij.org/ftp/Haskell/perfect-shuffle.txt
  // here's an O(n log n) solution:
  def shuffle[T](xs: List[T], r: scala.util.Random) = {
    xs.toStream.zip(Stream.continually(r.nextDouble _).map(_())).toList.sortWith(_._2 < _._2).map(_._1)
  }

  /* 2.8+
  //def shuffle[T](xs: List[T], r: java.util.Random) = {
   def shuffle[T](xs: scala.List[T])(implicit r: java.util.Random) = {
    xs.zip(Stream.continually(r.nextDouble)).sortWith(_._2 < _._2).map(_._1)
  }
  */

  def permute[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => scala.List(Nil)
    case _ => xs.flatMap(x => permute(xs.filter(_ != x)).map(x :: _))
  }

  // make a stream of x, f(x), f(f(x)), etc.
  // in Haskell this is called "iterate".  it ought to be in the standard library
  // as Stream.iterate.  "unfold" should be more general, but nonetheless I'm
  // going to call this unfold for the moment...
  def unfold[T](x:T)(f:T=>T):Stream[T] =
    Stream.cons(x,unfold(f(x))(f))

  def iterate[T](x:T)(f:T=>T):Stream[T] =
    Stream.cons(x,iterate(f(x))(f))
}

/*
// Haskell has these, dunno why they're not in Scala. some of these
  // could go on Iterable, actually, instead of Stream specifically
implicit def pimpMyStream[T](s1: Stream[T]): RichStream[T] = new RichStream(s1)
  class RichStream[T1](s1: Stream[T1]) {
    def zipWith[T2,T3](fn: (T1,T2)=>T3)(s2: Stream[T2]): Stream[T3] =
      if(s1.isEmpty || s2.isEmpty) Stream.empty
      else Stream.cons(fn(s1.head, s2.head), new RichStream(s1.tail).zipWith(fn)(s2.tail))
    def tails: Stream[Stream[T1]] =
      if(s1.isEmpty) Stream(Stream.empty)
      else Stream.cons(s1,s1.tail.tails)
    def inits: Stream[Stream[T1]] =
      if(s1.isEmpty) Stream(Stream.empty)
      else Stream.cons(Stream.empty, s1.tail.inits.map(Stream.cons(s1.head, _)))
    def circular: Stream[T1] = { lazy val c: Stream[T1] = s1.append(c); c }
    def break(fn: T1=>Boolean): (Seq[T1],Stream[T1]) = {
      val matched = new collection.mutable.ArrayBuffer[T1]
      var walk = s1
      while(!walk.isEmpty && fn(walk.head)) {
        matched += walk.head
        walk = walk.tail
      }
      (List(matched:_*), walk)
    }
    def group: Stream[Seq[T1]] =  // this is an unfold, right?
      if(s1.isEmpty) Stream.empty
      else break(_ == s1.head) match {
        case (segment, more) => Stream.cons(segment, more.group)
      }
    def scanl[T2](initial: T2)(fn: (T2, T1) => T2): Stream[T2] = {
      val next = fn(initial, s1.head)
      next #:: s1.tail.scanl(next)(fn)
    }
    def scanl1(fn: (T1, T1) => T1): Stream[T1] =
      s1.tail.scanl(s1.head)(fn)
  } 
*/
