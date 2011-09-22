package list;

abstract class MyList[A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def length: Int
  def ::[B >: A](item: B): MyList[B] =
    new MyListImpl(item, this.asInstanceOf[MyList[B]])
}

class MyListImpl[A](val head: A, val tail: MyList[A]) extends MyList[A] {
  def isEmpty = false
  def length: Int = 1 + tail.length
  override def toString: String = head + " " + tail
}

object MyListNil extends MyList[Nothing] {
  def head: Nothing = throw new Exception("head of empty list")
  def tail: MyList[Nothing] = throw new Exception("tail of empty list")
  def isEmpty = true
  def length = 0
  override def toString =  ""
}

object MyList {
  def apply[A](items: A*): MyList[A] = {
    var list: MyList[A] = MyListNil.asInstanceOf[MyList[A]]

    for (idx <- 0 until items.length reverse)
      list = items(idx) :: list
    list
  }

  def main(args : Array[String]) {

    var list1 = MyList("ABC", "WXYZ", "123")

    println(list1.head.length)
    //	res0: Int = 3

    var list2 = MyList("ABC", 123, 3.14159)
    //list: MyList[Any] = ABC 123 3.14159

    println(list2)
  }
}
