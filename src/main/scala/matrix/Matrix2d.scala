package matrix


object Matrix2dTest {
  def main(args : Array[String]) = {
    val m = Matrix2d(Array(
      Array(0.0, 1.0/3.0, 1.0/3.0, 1.0/3.0),
      Array(1.0/3.0, 0.0, 0.0, 2.0/3.0),
      Array(1.0/3.0, 0.0, 0.0, 2.0/3.0),
      Array(0.0, 1.0, 0.0, 0.0)
    ))

    val a0 = Matrix2d(Array(Array(0.0, 0.0, 1.0/2.0, 1.0/2.0)))
    val a1 = a0 mult m
    val a2 = a1 mult m
    val a3 = a2 mult m

    println("a1 = " + a1.toString)
    println("a2 = " + a2.toString)
    println("a3 = " + a3.toString)

    val mm = m mult m
    println("\nmm = " + mm.toString)

    val m2 = Matrix2d(Array(
      Array(0.826, 0.174),
      Array(0.600, 0.400)
    ))

    val m2m2 = m2 mult m2
    println("\nm2m2 = " + m2m2.toString)

    val m100 = (0 to 100).foldLeft(m)((a, b) => a mult m)
    println("\nm100 = " + m100.toString)
  }
}

trait MatrixB {
  def values : Array[_]
  def size : Int
  def xsize : Int
  def ysize : Int
  def col(n : Int) : Array[Double]
  def mult(m : MatrixB) : MatrixB
}
case class Matrix2d(values : Array[Array[Double]]) extends MatrixB {

  def mult(m : MatrixB) = {
    assert(m.ysize == this.xsize)
    Matrix2d(
      (0 until this.ysize).map(c =>
        (0 until this.xsize).map(r =>
          this.values(c).zip(m.col(r)).map(x => x._1 * x._2).sum
        ).toArray).toArray
    )
  }

  def transpose() = {
    // ...
  }

  def col(n : Int) : Array[Double] = values.map(v => v(n))

  val size = xsize * ysize
  val xsize = values(0).size
  val ysize = values.size

  override def toString : String = {
    values.map(r => r.mkString(" ")).mkString("\n")
  }
}

object Matrix2d {
  val NULL = new MatrixB {
    val values = Array()
    def mult(m : MatrixB) = this
    def col(n : Int) = Array[Double]()
    val size = 0
    val xsize = 0
    val ysize = 0
  }
}
