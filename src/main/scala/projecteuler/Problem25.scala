package projecteuler

object Problem25 {

  // define fibnacci stream
  lazy val fib: Stream[BigInt] =
      Stream.cons(0, Stream.cons(1, fib.zip(fib.tail).map(p => p._1 + p._2)))

  def main(args : Array[String]) {

    // zip with index will zip each element of the stream with an index
    // the map (_.swap) will swap the tuple arguments so that the index is the first element
    val fibI = fib.zipWithIndex map (_.swap)
    val r = fibI.find(f => f._2.toString().length >= 1000)

    println(r)

    // or zip a stream of numbers with the fib stream
    val fibI2 = (Stream from 0).zip(fib)
    val r2 = fibI2.find(f => f._2.toString().length >= 1000)

    println(r2)
  }
}
