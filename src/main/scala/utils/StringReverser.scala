package utils

object Main {

  def main(args: Array[String]) = {

    println(">>String reverser")

    val testStr1 = "987654321"
    println("test1: reversing " + testStr1 + " = " + reverse(testStr1))

    val xs = 1 :: 2 :: 3 :: Nil
    println("test2: reversing " + xs.mkString(",") + " = "
            + reverseList(xs).mkString(","))
    
    println("<<String reverser")
  }

  // simple string reversal
  def reverse(str : String) : String = {

    if ((str == null) || (str.length <= 1)) {
      str
    }
    else {
      reverse(str.substring(1, str.length)) + str.substring(0, 1);
    }
  }

  // reverseList
  def reverseList(lst : List[Any]) : List[Any] = lst match {

    case Nil => lst
    case x :: xs =>
      reverseList(xs) ::: List(x) // append to tail by making new list of x
  }
}
