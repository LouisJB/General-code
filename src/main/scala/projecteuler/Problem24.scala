package projecteuler

object Problem24 {

  // http://www.villainy.org/euler/tag/permutations/
  // http://en.wikipedia.org/wiki/Permutation#Lexicographical_order_generation
  // http://tafakuri.net/?p=68
  def main(args : Array[String]) {

    val s = " 0123456789"
    val r = perm(1000000, s.toCharArray)
    val x = r.dropRight(1)
    println("r = " + new String(x))


    //val p = permute(s.toList, 1000000)
    //val r2 = p drop 1000000 take 1

    //println("r = " + r2.mkString)

    //println(p.map(_.mkString).mkString(","))

    //println(p.map(_.mkString).mkString(","))
    //p.foreach(e => println(e.mkString))
  }


  def perm(k : Int, s : Array[Char]) : Array[Char] = {
    var n = s.length - 1
    var factorial = 1
    for (j <- 2 to n - 1) {             // compute (n- 1)!
       factorial = factorial * j
    }

    for (j <- 1 to (n - 1)) {
       val tempj = (k / factorial) % (n + 1 - j)
       val temps = s(j + tempj)
       for (i <- j + tempj to j + 1 by -1) {
           s(i) = s(i - 1)      // shift the chain right
       }
       s(j) = temps
       factorial = factorial / (n - j)
    }
    s
  }


  def permutate[T](ls : List[T]) : List[List[T]] = ls match {
    case List() => List()
    case List(a) => List(List(a))
    case _ => ls.flatMap(item => permutate(ls.filter(_ != item)).map
                (permutate => item :: permutate))
  }

//	def permute2[T](ls : List[T]) : List[List[T]] = {
//		permute2(ls, List(Nil))
//	}
//

  def permute2[T](ls : List[T], lls : List[List[T]]) : List[List[T]] = {
    ls match {
      case Nil => lls
      case (x :: xs)  =>
        permute2(xs, lls).map(y => y ::: x :: Nil ) ::: lls
    }
  }

  def permute[A](xs: List[A], n : Int): List[List[A]] = xs match {
    case Nil => List(Nil)
    case _ => xs.flatMap(x => permute(xs.filter(_ != x), n).map(x :: _))
  }
}
