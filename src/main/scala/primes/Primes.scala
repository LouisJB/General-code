package primes {

  /* Haskell...

    primes = sieve [2..]
    sieve (p : xs) = p : sieve [x | x <− xs, x `mod` p > 0]
  */
  object Primes1 {

    type IStream = Stream[Int]

    def primes : IStream = {

      var is = Stream from 2

      def sieve(numbers: IStream): IStream = {
        Stream.cons(
          numbers.head,
          sieve(for (x <- numbers.tail if x % numbers.head > 0) yield x))
      }

      sieve(is)
    }

    def main(args : Array[String]) = {

      primes take 100 foreach println
    }
  }

  object Primes2 {
    type IStream = Stream[Int]

    def primes = {
      def sieve(is: IStream): IStream = {
        val h = is.head
        Stream.cons(h, sieve(is filter (_ % h > 0)))
      }

      sieve(Stream.from(2))
    }

    def main(args: Array[String]) {
      println(primes take 100 toList)
    }
  }
}
