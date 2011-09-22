package primes


object PrimesLazyTrait {
  
  sealed trait Lazy[+A] {
    def head: A = this match {
      case Nil => error("head called on empty")
      case Cons(h, _) => h()
    }

    def tail: Lazy[A] = this match {
      case Nil => error("tail on empty")
      case Cons(_, t) => t()
    }

    def filter(p: A => Boolean): Lazy[A] = this match {
      case Nil => Nil
      case Cons(h, t) => if(p(h())) Cons(h, () => t() filter p) else t() filter p
    }

    def foreach(f: A => Unit) {
      this match {
        case Nil =>
        case Cons(h, t) => {
          f(h())
          t() foreach f
        }
      }
    }

    def toList: List[A] = this match {
      case Nil => scala.Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  final case class Cons[+A](h: () => A, t: () => Lazy[A]) extends Lazy[A]
  case object Nil extends Lazy[Nothing]

  def from(n: Int): Lazy[Int] = Cons(() => n, () => from(n + 1))

  def primes = {
    def sieve(is: => Lazy[Int]): Lazy[Int] = {
      lazy val h = is.head
      Cons(() => h, () => sieve(is filter (_ % h > 0)))
    }

    sieve(from(2))
  }

  def main(args: Array[String]) {
    primes foreach println
  }
}
