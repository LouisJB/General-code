package projecteuler

import utils.Primes

object Problem50 {

// Which prime, below one-million, can be written as the sum of the most consecutive primes?


  def main(args : Array[String]) {

    //val ps = primes2.takeWhile(_ < 1000000).toList

    val ps = (1 to 1000000).filter(n => Primes.isPrime(n)).toList

    println("p = " + ps.tail)

    val r = findMax(ps)

    println(r)
  }

  def findMax(ls : List[Int]) : Int = {

    var maxLen = 0
    var len = 0
    var max = 0
    var sum = 0
    var xs = ls

    while (xs.length > 0) {
      //println("s = " + xs.head)
      sum = 0
      len = 0
      for (p <- xs if (sum < ls.last && p < ls.last/2)) {
        //if (sum < ls.last) {
        {
          //println("adding p = " + p + " to sum = " + sum)
          sum = sum + p
          len += 1
          if (ls.contains(sum) && len > maxLen) {
            maxLen = len
            max = sum
            println("start " + xs.head + ", max = " + max + ", maxLen = " + maxLen);
          }
          else { //println("no for " + sum)
          }
        }
      }
      xs = xs.tail
      println("next" + xs.head)
    }

    max
  }

  def primes : Stream[Int] = {

    var is = Stream from 2

    def sieve(numbers: Stream[Int]): Stream[Int] = {
      Stream.cons(
         numbers.head,
        sieve(for (x <- numbers.tail if x % numbers.head > 0) yield x))
    }

    sieve(is)
  }

  def primes2 = {
   def sieve(is: Stream[Int]): Stream[Int] = {
     val h = is.head
     Stream.cons(h, sieve(is filter (_ % h > 0)))
   }

   sieve(Stream.from(2))
  }
}
