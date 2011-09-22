package projecteuler

import java.lang.Math._

object Project7 {

  def main(args : Array[String]) {

    def isPrime(n: Int) = (2 to (ceil(sqrt(n))).toInt) forall (n % _ != 0)
    //def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

    var i : Int = 1
    var j : Int = 2
    while (i <= 10001) {
      if (isPrime(j)) {
        print(i)
        print(" : ")
        println(j)
        i = i + 1
      }
      j = j + 1
    }
  }

  def main2(args : Array[String]) {

    def from(n: Int): Stream[Int] =
            Stream.cons(n, from(n + 1))

    def sieve(s: Stream[Int]): Stream[Int] =
            Stream.cons(s.head, sieve(s.tail filter { _ % s.head != 0 }))

    def primes = sieve(from(2))

  /*
  primes take 1000 print

  type IStream = Stream[Int]

  def primes2 = {
   def sieve(is: IStream): IStream = {
     val h = is.head
     Stream.cons(h, sieve(is filter (_ % h > 0)))
   }

   sieve(Stream.from(2))
  }
  */
    //println(primes2 take 10001 toList)

    primes take 10001 print
  }
}
