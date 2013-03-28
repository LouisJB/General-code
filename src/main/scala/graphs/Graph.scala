package graphs

import scala.collection.mutable.PriorityQueue
import scala.swing.Applet
import scala.swing.Reactor

import javax.swing._
import java.awt.Graphics
import java.awt.Color

class Vertex(
  lbl : String,
  adj : => List[Edge],
  xcoord : Int,
  ycoord : Int) {

  val label = lbl
  lazy val adjacencies = adj
  val x = xcoord
  val y = ycoord

  // mutable working data
  var distance = Int.MaxValue
  var predecessor : Option[Vertex] = None

  override def toString = { "Vertex: label: " + label + ", distance: " + distance }
}

class Edge(
  lbl : String,
  toVtx : Vertex,
  wgt : Int) {

  def this(lbl : String, toVtx : Vertex) = this(lbl, toVtx, 0)

  val label = lbl
  lazy val adjacentVertex = toVtx
  val weight = wgt

  override def toString = { "Edge: label: " + label + ", weight: " + weight }
}


object GraphApp {

  def main(args : Array[String]) {

    val applet = new GraphApplet()
    val frame = new JFrame()
    frame.setBounds(50, 50, 750, 750)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    frame.add(applet)

    frame.setVisible(true)
    applet.init
    applet.start
  }

  def primsMst(vertices : List[Vertex]) = {

    val source = vertices(0)
    source.distance = 0

    //var vQ : PriorityQueue[Vertex] = new PriorityQueue[Vertex]()(
    //  new Ordering[Vertex] {
    //    def compare(n: Vertex, y : Vertex) : Int = y.distance compare n.distance})

    var vQ : PriorityQueue[Vertex] = new PriorityQueue[Vertex]()(
      Ordering.fromLessThan[Vertex](_.distance > _.distance))

    vertices.head.distance = 0 // set source vertex
    //println(vertices.head.toString)
    vQ ++= vertices
    
    println("Queue size = " + vQ.length)

    while (!vQ.isEmpty) {

      val u = vQ.dequeue
      println(u.toString)

      //println("vQ.length = " + vQ.length)

      for (e <- u.adjacencies) {

        println(e.toString)

        val v = e.adjacentVertex

        println(" - adjacent vertex : " + v.toString)

        val weight = e.weight + u.distance

        if (vQ.toList.contains(v) && weight < v.distance) {

          //val temp = vQ.dequeue

          //println("vQ.length = " + vQ.length)
          v.predecessor = Some(u)
          v.distance = weight  // set distance weight value

          println("Best weight for vertex: " + v.toString)

          // rebuild the PQ
          val tempQ = vQ.toQueue
          vQ.clear
          vQ ++= tempQ
          //vQ += v
          //println("vQ.length = " + vQ.length)
        }
      }
    }
  }

  def printAllPaths(g : List[Vertex], s : Vertex) {
    println("printAllPaths from vertex : " + s.toString)
    println
    for (vertex <- g) printPath(s, vertex)
  }

  // trace along predecessor path from v to s recursively (output from s to v)
  def printPath(s : Vertex, v : Vertex) {
    if (v == s) {
        println("At source: " + s.toString)
    }
    else {
      v.predecessor match {
        case None => println("No path from " + s + " to " + v + " exists")
        case Some(u) => {
          printPath(s, u)
          println(" to " + v  + " - cost = " + v.distance)
        }
      }
    }
  }
}

class GraphApplet extends Applet {

  val vsize = 10

  lazy val v1 = new Vertex("a", e12 :: e13 :: e14 :: Nil, 100, 200)
  lazy val v2 = new Vertex("b", e21 :: e23 :: e25 :: Nil, 200, 300)
  lazy val v3 = new Vertex("c", e31 :: e32 :: e34 :: Nil, 300, 200)
  lazy val v4 = new Vertex("d", e41 :: e43 :: Nil, 200, 100)
  lazy val v5 = new Vertex("e", e52 :: Nil, 300, 300)

  lazy val e12 : Edge = new Edge("e1  ", v2, 2)
  lazy val e21 : Edge = new Edge("e1  ", v1, 2)

  lazy val e23 : Edge = new Edge("e2  ", v3, 4)
  lazy val e32 : Edge = new Edge("e2  ", v2, 4)

  lazy val e31 : Edge = new Edge("e3  ", v1, 3)
  lazy val e13 : Edge = new Edge("e3  ", v3, 3)

  lazy val e41 : Edge = new Edge("e4  ", v1, 8)
  lazy val e14 : Edge = new Edge("e4  ", v4, 8)

  lazy val e52 : Edge = new Edge("e5  ", v2, 5)
  lazy val e25 : Edge = new Edge("e5  ", v5, 5)

  lazy val e34 : Edge = new Edge("e6  ", v4, 2)
  lazy val e43 : Edge = new Edge("e6  ", v3, 2)

  val graph : List[Vertex] = v1 :: v2 :: v3 :: v4 :: v5 :: Nil

  object ui extends UI with Reactor {

    def init() = {
      GraphApp.primsMst(graph)
      GraphApp.printAllPaths(graph, graph.head)
    }

    override def start() = {
      this.repaint()
    }
  }

  override def paint(g : Graphics) = {
    graph.foreach(v => draw(g, v))
    printAllPaths(g, graph, graph.head)
  }

  def draw(g : Graphics, v : Vertex) {

    val x = v.x
    val y = v.y

    g.setFont(g.getFont().deriveFont(7))

    // draw incident edges
    g.setColor(Color.GRAY)

    v.adjacencies.foreach(e => {
        val xx2 = e.adjacentVertex.x
        val yy2 = e.adjacentVertex.y
        
        val x1 = offset(x, xx2, vsize)
        val y1 = offset(y, yy2, vsize)
        val x2 = offset(xx2, x, vsize)
        val y2 = offset(yy2, y, vsize)

        g.drawLine(x1, y1, x2, y2)
        
        val xmid = (x1 + x2) / 2
        val ymid = (y1 + y2) / 2

        val edgeLbl = e.label + "(" + e.weight + ")"
        g.drawChars(edgeLbl.toCharArray, 0, edgeLbl.length, xmid + vsize, ymid + vsize)
    })

    //draw vertices
    g.setColor(Color.BLUE)

    g.fillArc(x - vsize/2, y - vsize/2, vsize, vsize, 0, 360)

    g.drawChars(v.label.toCharArray, 0, v.label.length, x + vsize, y + vsize)
  }

  def printAllPaths(gr : Graphics, g : List[Vertex], s : Vertex) {
    def printPath(s : Vertex, v : Vertex) {
      if (v != s) {
        v.predecessor match {
          case None =>
          case Some(u) => {
            printPath(s, u)
            gr.setColor(Color.GREEN)
            gr.drawLine(
              offset(v.x, u.x, vsize) + 2,
              offset(v.y, u.y, vsize) + 1,
              offset(u.x, v.x, vsize) + 2,
              offset(u.y, v.y, vsize) + 1)
          }
        }
      }
    }
    for (vertex <- g) printPath(s, vertex)
  }

  def offset(x1 : Int, x2 : Int, size : Int) = {
    if (x1 == x2) x1 else if (x1 < x2) x1 + size/2 else x1 - size/2
  }
}
