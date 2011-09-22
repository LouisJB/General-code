package datastructures
//package scala.collection
//package mutable

import scala.collection.mutable.{Cloneable, ArrayBuffer, Builder, Queue}
import scala.collection.{SeqLike, TraversableOnce}
//import scala.collection.generic._

//import annotation.migration


/** This class implements priority queues using a (min/max)Heap.
 *  To prioritize elements of type T there must be an implicit
 *  Ordering[T] available at creation.
 *
 *  @tparam A    type of the elements in this priority queue.
 *  @param ord   implicit ordering used to compare the elements of type `A`.
 *
 *  @author  Louis Botterill
 *  @version 1.0, 04/04/2010
 *  @since   1
 *
 *  @define Coll PriorityQueue
 *  @define coll priority queue
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */

@serializable @cloneable
class Heap[A]/*(implicit manifest : Manifest[A],*/(implicit ord : Ordering[A])
      extends Seq[A] 
      with collection.SeqLike[A, Heap[A]]
      //with Growable[A]
      with Cloneable[Heap[A]]
      with Builder[A, Heap[A]] {

  import ord._

  private var heapSize = 0                    // current heap size
  private var array = new ArrayBuffer[A](10)  // array to hold heap

  // helpers to locate parent and children by array index
  private def getParent(n : Int) = (n - 1) / 2
  private def getLeft(n : Int) = 2 * n + 1
  private def getRight(n : Int) = 2 * n + 2

  // for testing, to be removed?
  def setElems(xs : TraversableOnce[A]) {
    xs.copyToBuffer(array)
    heapSize = xs.size
    buildHeap
  }

  /** Returns an iterator which yields all the elements of the priority
   *  queue in priority order.
   *
   *  @return  an iterator over all elements sorted in priority order.
   */
  override def iterator: Iterator[A] = new Iterator[A] {
    val as = array.clone
    var i = heapSize
    def hasNext: Boolean = i > 0
    def next(): A = {
      val res = as(0)
      as(0) = as(i)
      i = i - 1
      heapify(as, 1, i)
      res
    }
  }

  /** Override foreach
   */
  override def foreach[U](f: A => U) {
    var i = 0
    while (i < heapSize) {
      f(array(i))
      i += 1
    }
  }

  /** Update by rebuilding from original with new element added
   *  (todo - more eficient way to do this using increaseKey?)
   *
   *  @param    idx       index of the element to update
   *  @param    elem      new element to update original
   */
  def update(idx : Int, elem : A) {
    if (idx < 0 || idx >= size)
      throw new IndexOutOfBoundsException("Indices must be between zero and size - 1")

    var i = 0
    val iter = iterator
    clear
    while (iter.hasNext) {
      val curr = iter.next
      if (i == idx) this += elem
      else this += curr
      i += 1
    }
  }

  /** returns element at given index
   *  or throws IndexOutOfBoundsException
   *
   *  @param   idx       starting index
   *
   *  @return  element at given index
   */
  def apply(idx : Int) = {
    if (idx < 0 || idx >= heapSize)
      throw new IndexOutOfBoundsException("Indices must be between zero and size - 1")

    var left = idx
    val iter = iterator
    var curr = iter.next
    while (left > 0) {
      curr = iter.next
      left -= 1
    }
    curr
  }

  /** build heap property in the whole queue
   */
  def buildHeap {
    for (i <- heapSize/2 to 0 by -1) heapify(i)
  }

  /** build heap property in the queue from index i down
   *
   *  @param   i         starting index
   */
  private def heapify(i : Int) {
    heapify(array, i, heapSize)
  }

  /** build heap property in the queue from index i down
   *
   *  @param   arr     Array to act on
   *  @param   i         starting index
   *  @param   hSize     current heap size
   */
  def heapify(arr : ArrayBuffer[A], i : Int, hSize : Int) {
    var largest = 0
    val l = getLeft(i)
    val r = getRight(i)
//println("comparing: hSize = " + hSize + ", l = " + l + ", r = " + r + ", heap = " + this.toString)
    if (l < hSize && arr(l) > arr(i))
      largest = l
    else
      largest = i

    if (r < hSize && arr(r) > arr(largest))
      largest = r

    if (largest != i) {
      swap(arr, i, largest)
      heapify(arr, largest, hSize)
    }
  }

  /** Increase the priority of the element
   *  or throws InxesOutOfBoundsException / IllegalArgumentException
   *  New key Value must be higher priority (>) in accordance with the
   *  provided Ordering[A]
   *
   *  @param   i         index ordinal into the queue
   *  @param   key       new key value
   */
  def increaseKey(i : Int, key : A) {
    if (i < 0 || i >= heapSize)
      throw new IndexOutOfBoundsException("Indices must be nonnegative and lesser than the size.")

    var n = i
    if (key < array(n)) {
      throw new IllegalArgumentException("Invalid key (" + key.toString + ")")
    }
    else {
      array(n) = key
      while (n > 0 && array(getParent(n)) < array(n)) {
        swap(array, n, getParent(n))
        n = getParent(n)
      }
    }
  }

  /** Inserts an element into the queue
   *
   *  @param  key        the key to add to the queue
   */
  def heapInsert(key : A) {
    heapSize = heapSize + 1
    array.append(key)
    increaseKey(heapSize - 1, key)
  }

  /** Adds all elements to the queue.
   *
   *  @param  elems       the elements to add.
   */
  def enqueue(elems: A*) { this ++= elems }

  /** Removes and returns the highest priority element from the queue
   *  or throws an error if there is no element contained in the queue
   *
   *  @return  element with highest priority
   */
  def dequeue() : A = {
    if (heapSize >= 1) {
      val max = array(0)
      array(0) = array(heapSize - 1)
      heapSize = heapSize - 1
      heapify(0)
      max
    } else
      throw new NoSuchElementException("heap underflow")
  }

  /** Returns the element with the highest priority in the queue,
   *  or throws an error if there is no element contained in the queue.
   *
   *  @return   the element with the highest priority.
   */
  def max: A = 
    if (heapSize >= 1) array(0)
    else throw new NoSuchElementException("queue is empty")

  /** Removes all elements from the queue. After this operation is completed,
   *  the queue will be empty.
   */
  def clear() { heapSize = 0 }

  override def length : Int = heapSize
  override def size : Int = length
  override def isEmpty : Boolean = heapSize < 1

  // builder
  def result = clone

  /** This method clones the priority queue.
   *
   *  @return  a priority queue with the same elements.
   */
  override def clone(): Heap[A] = new Heap[A] ++= this.iterator


  /** Inserts a single element into the priority queue.
   *
   *  @param    elem      the element to insert
   *  @return   this      this object with new element inserted
   */
  def +=(elem: A): this.type = {
    heapInsert(elem)
    this
  }

  /** Adds all elements provided by an <code>Iterable</code> object
   *  into the priority queue.
   *
   *  @param  iter        an iterable object
   *  @return copy of this object with new elements added
   */
  def ++(xs: TraversableOnce[A]) = { this.clone() ++= xs }

  /** Returns a regular queue containing the same elements.
   */
  def toQueue: Queue[A] = new Queue[A] ++= this.iterator

  // for TraversableLike
  protected[this] override def newBuilder = new Heap[A]


  @deprecated(
    "Use += instead if you intend to add by side effect to an existing collection.\n"+
    "Use `clone() +=' if you intend to create a new collection."
  )
  def +(elem: A): Heap[A] = { this.clone() += elem }

  /** Add two or more elements to this set.
   *
   *  @param    elem1 the first element.
   *  @param    kv2 the second element.
   *  @param    kvs the remaining elements.
   */
  @deprecated(
    "Use ++= instead if you intend to add by side effect to an existing collection.\n"+
    "Use `clone() ++=' if you intend to create a new collection."
  )
  def +(elem1: A, elem2: A, elems: A*) = { this.clone().+=(elem1, elem2, elems : _*) }


  // helpers

  /** swaps elements in place in an ArrayBuffer
   *
   *  @param  arr       the array
   *  @param  i         the 1st array index to swap
   *  @param  j         the 2nd array index to swap
   */
  private def swap[T](arr : ArrayBuffer[T], i : Int, j : Int) {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  }

  /**
   * Overriden toString
   *
   * @return  string representation of this object
   */
  override def toString = "heap: " + array.toList.take(heapSize).mkString("Heap(", ", ", ")")

  /** The hashCode method always yields an error, since it is not
   *  safe to use mutable queues as keys in hash tables.
   */
  override def hashCode(): Int =
    throw new UnsupportedOperationException("unsuitable as hash key")
}


// test code, to be moved to scalatest/check test cases
object TestHeap {

  implicit def ord[A] = Ordering.fromLessThan[Int](_ < _)
  
  def main(args : Array[String]) {

    val heap = new Heap[Int]()
    
    heap.enqueue(3, 2, 3, 8, 5, 6)
    
    println(heap)

    heap.heapInsert(4)

    println(heap)


    import TestObj._

    var heap2 = new Heap[MyVal]()(/*implicitly[Manifest[MyVal]],*/
      Ordering.fromLessThan[MyVal](_.key < _.key))

    heap2 ++= List(3, 2, 3, 8, 5, 6).map(MyVal(_))

    println(heap2)

    heap2.heapInsert(MyVal(42))

    println(heap2)

    heap2.increaseKey(3, MyVal(33))

    println(heap2)

    println(heap2.max)

    println(heap2)

    println(heap2.dequeue())

    println(heap2)

    while (heap2.size > 0) {
      val m = heap2.dequeue
      println(m + "," + heap2.toString)
    }

    try {
      heap2.dequeue
    }
    catch {
      case e : NoSuchElementException => println("underflow ok")
    }

    heap.clear
    println(heap)
    heap.setElems(List(1,2,3))
    println(heap)
    println(heap.max)

    while (heap.size > 0) {
      val m = heap.dequeue
      println(m + "," + heap.toString)
    }


    val heap3 = new Heap[MyVal]()(/*manifest[MyVal],*/
      Ordering.fromLessThan[MyVal](_.key > _.key))

    heap3.setElems(List(3, 2, 3, 8, 5, 6).map(MyVal(_)))

    println(heap3)

    heap3.heapInsert(MyVal(42))

    println(heap3)

    heap3.increaseKey(3, MyVal(1))

    println(heap3)

    println(heap3.dequeue)
  }
}

object TestObj {
  
  case class MyVal(key: Int) {

    override def toString = key.toString
  }

  implicit def ord1(v: MyVal): Ordered[MyVal] = new Ordered[MyVal] {
    def compare(other: MyVal) = v.key.compare(other.key)
  }

  implicit def ord2[A] = Ordering.fromLessThan[MyVal](_.key < _.key)
}
