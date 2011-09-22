package Sorting

object Sorting {

  // quick sort
  def qsort(xs: List[Int]): List[Int] =
    if (xs.length < 2) xs
    else {
      val pivot = xs(xs.length / 2)
      qsort(xs.filter(_ < pivot)) :::
        xs.filter(_ == pivot) :::
        qsort(xs.filter(_ > pivot))
    }

  def quicksortI(ar : Array[Int]) : Array[Int] = quicksortI(ar, 0, ar.length - 1)

  // quick sort imperatively
  def quicksortI(ar : Array[Int], p : Int, q : Int) : Array[Int] = {
    if (p < q) {
      val pivot = partI(ar, p, q)
      quicksortI(ar, p, pivot - 1)
      quicksortI(ar, pivot + 1, q)
    }
    ar
  }

  def partI(vs : Array[Int], p : Int, r : Int) = {
    var i = p
    for (j <- (p + 1 to r)) {
      if (vs(j) < vs(p)) {
        i = i + 1
        swap(vs, j, i)
      }
    }
    swap(vs, p, i)
    i
  }

  def swap[T](vs : Array[T], a : Int, b : Int) = {
    val t = vs(a)
    vs(a) = vs(b)
    vs(b) = t
  }

  def main(args : Array[String]) {

    val ar = Array(5, 7, 3, 5, 2, 3, 1, 9, 1, 5, 2, 0)
    println("Array = " + ar.toList.mkString(", "))
    println("quicksortI = " + quicksortI(ar.clone).toList.mkString(", "))
    println("qsort = " + qsort(ar.toList).mkString(", "))

    val ls = List(5, 3, 7, 6, 8, 9, 1, 4)
  }
}
