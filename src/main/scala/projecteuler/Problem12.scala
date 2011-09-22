package projecteuler

object Problem12 {

  def main(args : Array[String]) {

    println("result = " + problem12(500))
  }

  def problem12(target : Int) {

    // using Stream.iterate to create a lazy stream of triangle numbers
    val trianglesI = Stream.iterate((1L,0L)){
            case (n, sum) => (n + 1L, sum + n) }.map(_._2)

    // test it
    //println("trianglesI take(5)")
    //trianglesI.take(5).foreach(println)

    // count the divisors (not massively efficient, but sufficient)
    def numDivisors(n : Long) = {

      var count = 0

      for (i <- 1L to math.sqrt(n).toInt) {
              if (n % i == 0) {
                      count = count + 2
              }
      }
      count
    }

    trianglesI.find(p => numDivisors(p) > target)
  }

  def otherTriangleStreams {

    // use a stream map and the multiplicative solution for triangle numbers
    val trianglesM = Stream.from(1).map(n => n * (n + 1) / 2)

    println("trianglesM take(5)")
    trianglesM.take(5).foreach(println)

    // using inits (not efficient)
    // inits -> inits(Stream.from(1)) is Stream(Stream(), Stream(1), Stream(1, 2), Stream(1, 2, 3), ...)
    // tails -> tails(Stream(1)) = Stream(Stream(1,2,3,...), Stream(2,3,4,...), Stream(3,4,5,...), ...)
    def inits[T](xs: Stream[T]) = Stream.from(0).map(xs.take(_))

    val trianglesInits = inits(Stream.from(1)).tail.map(_.sum)

    println("trianglesInits take(5)")
    trianglesInits.take(5).foreach(println)

    // using zipWith
    def zipWith[T1,T2,T3](s1: Stream[T1], s2: Stream[T2])(fn: (T1,T2) => T3) = s1.zip(s2).map(Function.tupled(fn))

    lazy val trianglesZ: Stream[Int] = Stream.cons(1, zipWith(trianglesZ, Stream.from(2))(_ + _))

    println("trianglesZ take(5)")
    trianglesZ.take(5).foreach(println)
  }
}
