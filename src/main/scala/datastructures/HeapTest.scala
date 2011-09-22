package datastructures

import org.scalacheck._
import org.scalacheck.ConsoleReporter.testStatsEx
import org.scalacheck.Test.check
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import java.util.{LinkedList, Arrays}


object CheckHeap extends Properties("Heap") {

  property("max") = Prop.forAll((h : Heap[Int]) => {
      
      if (h.size > 0) {
        h.toList.max == h.dequeue
      }
      else {
        h.isEmpty
      }
  })

  property("+=") = Prop.forAll((h : Heap[Int], key : Int) => {

    h += key
    h.toList.max == h.dequeue
  })

  property("increaseKey") = Prop.forAll((h : Heap[Int]) => {

    if (h.size > 0) {
      val max = h.max
      if (max < Int.MaxValue) {
        h.increaseKey(h.size-1, max + 1)
        h.max == max + 1
      }
      else {
        try {
          h.increaseKey(h.size-1, max + 1)
          false
        }
        catch {
          case e : IllegalArgumentException => true
          case _ => false
        }
      }
    }
    else true
  })

  property("length") = Prop.forAll((h : Heap[Int]) => h.length == h.toList.length)
  
  property("size") = Prop.forAll((h : Heap[Int]) => h.size == h.toList.size)

  property("clear") = Prop.forAll((h : Heap[Int]) => {
      h.clear
      try {
        h.dequeue
        false
      }
      catch {
        case ex : NoSuchElementException => h.length == 0
        case _ => false
      }
  })


  // implicit conversion to Arbitrary[Heap[T]]
  //   from List[T]
  implicit def arbMaxHeap[T <: Int : Manifest : Arbitrary] : Arbitrary[Heap[T]] = {
      Arbitrary {
        arbitrary[List[T]].map(ls => {
            implicit val o = Ordering.fromLessThan[T](_ < _) // max heap

            val x = new Heap[T]()
            x ++= ls
            x
          })
      }
  }
}
