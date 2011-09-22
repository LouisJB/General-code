package mapreduce

object FunctionComposition {

  def main(args : Array[String]) {

    println(reduce(add, 0, List(1, 2, 3, 4)))
  }

  def add(x : Int, y : Int) = x + y
  def product(x : Int, y : Int) = x * y

  def reduce(f:(Int, Int) => Int, x : Int, ls : List[Int]) : Int = ls match {
    case Nil => x
    case (a :: as) => f(a, reduce(f, x, as))
  }

  def map(f:(Int, Int) => Int, x : Int, ls : List[Int]) = {

    //reduce(cons.andThen(f), Nil, ls)
  }
}
