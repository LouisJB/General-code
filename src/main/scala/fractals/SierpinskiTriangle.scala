package fractals

import scala.swing.Applet
import scala.swing.Reactor

import javax.swing.JFrame
import java.awt.Graphics
import java.awt.Point

class SierpinskiTriangleApplet extends Applet {

  object ui extends UI with Reactor {

    def init() = {}

    override def start() = {
      this.repaint()
    }
  }

  override def paint(g : Graphics) = {
    val b = g.getClipBounds()
    val p1 = new Point((b.x + b.width) / 2, b.y)
    val p2 = new Point(0, b.y + b.height)
    val p3 = new Point(b.x + b.width , b.y + b.height)

    drawSierpinski(g, p1, p2, p3)
  }

  def drawSierpinski(g : Graphics, p1 : Point, p2 : Point, p3 : Point) {

    g.drawLine(p1.x, p1.y, p2.x, p2.y)
    g.drawLine(p1.x, p1.y, p3.x, p3.y)
    g.drawLine(p2.x, p2.y, p3.x, p3.y)

    draw(g,
        new Point((p1.x + p2.x) / 2, (p1.y + p2.y) / 2),
        new Point((p1.x + p3.x) / 2, (p1.y + p3.y) / 2),
        new Point((p2.x + p3.x) / 2, (p2.y + p3.y) / 2))
  }

  def draw (g : Graphics, p1 : Point, p2 : Point, p3 : Point) {

    if (p1.x != p2.x) {
      g.drawLine(p1.x, p1.y, p2.x, p2.y)
      g.drawLine(p1.x, p1.y, p3.x, p3.y)
      g.drawLine(p2.x, p2.y, p3.x, p3.y)

      draw(g,
            new Point((p1.x + p2.x) / 2 + (p2.x - p3.x) / 2, //x coordinate of first corner
            (p1.y + p2.y) / 2 + (p2.y - p3.y) / 2), //y coordinate of first corner
            new Point((p1.x + p2.x) / 2 + (p1.x - p3.x) / 2, //x coordinate of second corner
            (p1.y + p2.y) / 2 + (p1.y - p3.y) / 2), //y coordinate of second corner
            new Point((p1.x + p2.x) / 2, //x coordinate of third corner
            (p1.y + p2.y) / 2)  //y coordinate of third corner
        )

       draw(g,
            new Point((p3.x + p2.x) / 2 + (p2.x - p1.x) / 2, //x coordinate of first corner
            (p3.y + p2.y) / 2 + (p2.y - p1.y) / 2), //y coordinate of first corner
            new Point((p3.x + p2.x) / 2 + (p3.x - p1.x) / 2, //x coordinate of second corner
            (p3.y + p2.y) / 2 + (p3.y - p1.y) / 2), //y coordinate of second corner
            new Point((p3.x + p2.x) / 2, //x coordinate of third corner
            (p3.y + p2.y) / 2)  //y coordinate of third corner
        )
    }
  }
}

object SierpinskiTriangle {

  def main(args : Array[String]) {

    val applet = new SierpinskiTriangleApplet()
    val frame = new JFrame()
    frame.setBounds(50, 50, 750, 750)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    frame.add(applet)

    frame.setVisible(true)
    applet.init
    applet.start
  }
}
