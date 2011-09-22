package quicksort;

object QuickSort {

  // Iterative/recursive quicksort
  def sortI[E <% Ordered[E]](xs: List[E]) : List[E] = {

    if (xs.isEmpty || xs.tail.isEmpty) xs
    else {

      val pivot = xs(xs.length / 2)

      // initialize lists
      var lows: List[E] = Nil
      var mids: List[E] = Nil
      var highs: List[E] = Nil

      // sort items into low, mid or high lists
      for( item <- xs) {

        if ( item == pivot) mids = item :: mids
        else if (item < pivot) lows = item :: lows
        else highs = item :: highs
      }

      // create sorted list appending sorted sub-lists
      sortI(lows) ::: mids ::: sort(highs)
    }
  }

  // recursive sort
  def sort[E <% Ordered[E]](xs: List[E]): List[E] = xs match {
    case List() => xs
    case _ => {
      sort(for(item <- xs.tail if item < xs.head) yield item) :::
        List(xs.head) :::
        sort(for(item <- xs.tail if item >= xs.head) yield item)
    }
  }

  def sort(xs: List[Int]): List[Int] = {
    if (xs.length < 2)
      xs
    else {
      val pivot = xs(xs.length / 2)
      sort(xs.filter(_ < pivot)) :::
           xs.filter(_ == pivot) :::
           sort(xs.filter(_ > pivot))
    }
  }

  def main(args : Array[String]) {

    val r1 = sort[String]( List( "Berlin", "Paris", "Barcelona", "Amsterdam"))

    assert(r1.equals(List( "Amsterdam", "Barcelona", "Berlin", "Paris")))

    val r2 = sort[String]( List( "Berlin", "Paris", "Barcelona", "Amsterdam"))

    assert(r2.equals(List( "Amsterdam", "Barcelona", "Berlin", "Paris")))
  }
}
