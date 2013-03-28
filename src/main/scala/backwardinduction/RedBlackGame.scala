package backwardinduction

import math._

/**
 * calculate the value of a simple game
 * from a pack of shuffled cards (equal red and black), draw 1 at a time.
 * if red, payoff = 1, if black, payoff = -1
 * can stop at any point. What is the 'value' of the game?
 */
object RedBlackGame extends App {

  def calcValue(size : Int) : Double = {
    val g : Array[Array[Double]] = Array.tabulate(size, size)(
      (y, x) => if (y == 0 && x >= 1) x.toDouble-1 else if (x <= 1 && y >= 1) y.toDouble-1 else 0.0)

    //g.map(n => n.mkString(", ")).foreach(println)
    //println()

    for (x <- 2 until g.size; y <- 2 until g(0).size) g(y)(x)
     = max(0,
        (((g(0)(x) / (g(0)(x) + g(y)(0)) * (-1 + (g(y)(x-1)))))
      + (((g(y)(0) / (g(0)(x) + g(y)(0)) * (1 + g(y-1)(x)))))))

    //g.map(n => n.mkString(", ")).foreach(println)

    //println("max val = " + g(size-1)(size-1))
    g(size-1)(size-1)
  }

  (1 to 1000 by 10).map(x => (x, calcValue(x))).foreach(println)
}
