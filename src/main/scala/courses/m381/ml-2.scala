package courses.m381

/*
#!/bin/sh
exec scala "$0" "$@"
!#
*/

object PrimitiveRecursion {
  
  // ***** Example 1.2 add(n, m)
  def g12(n1 : Int, n2 : Int, n3 : Int) : Int = n3 + 1
  def add(n : Int, m : Int) : Int = if (m == 0) n else g12(n, m, add(n, m - 1))
  
  // ***** Example 1.3 mult(n, m)
  def g13(n1 : Int, n2 : Int, n3 : Int) : Int = n3 + n1
  def mult(n : Int, m : Int) : Int = if (m == 0) 0 else g13(n, m, mult(n, m - 1))

  // ***** Example 1.4 exp(n, m)
  def exp(n : Int, m : Int) : Int = if (m == 0) 1 else mult(exp(n, m - 1), n)

  def pred(n : Int) = if (n == 0) 0 else n-1

  def cutOffSub(n : Int, m : Int) : Int = if (m == 0) n else pred(cutOffSub(n, m-1))

  def main(args : Array[String]) {

    println("example 1.2, add(5, 6) = " + add(5, 6))

    println("example 1.3, mult(5, 6) = " + mult(5, 6))

    println("example 1.4, exp(5, 6) = " + exp(5, 6))

    println("example 1.5, cutOffSub(6, 7) = " + cutOffSub(6, 7))

    println("example 1.5, cutOffSub(6, 4) = " + cutOffSub(6, 4))
  }
}

//PrimitiveRecursion.main(args)

