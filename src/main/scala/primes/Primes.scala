package primes {

  /* From Haskell...
     primes = sieve [2..]
     sieve (p : xs) = p : sieve [x | x <− xs, x `mod` p > 0]
  */

  object Primes1 {
    def primes : Stream[Int] = {
      def sieve(is : Stream[Int]): Stream[Int] = {
        Stream.cons(
          is.head,
          sieve(for (x <- is.tail if x % is.head > 0) yield x))
      }
      sieve(Stream from 2)
    }

    def main(args : Array[String]) = {
      primes take 100 foreach println
    }
  }

  object Primes2 {
    def primes = {
      def sieve(is: Stream[Int]): Stream[Int] = {
        val h = is.head
        Stream.cons(h, sieve(is filter (_ % h > 0)))
      }
      sieve(Stream.from(2))
    }

    def main(args: Array[String]) {
      primes take 100 foreach println
    }
  }
}
