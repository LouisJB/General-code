#!/bin/sh
exec scala -deprecation "$0" "$@"
!#

object PrimitiveRecursion {

  // ***** Problem 3.1
  // The function fac : N −→ N given by fac(n) = n! is defined by primitive 
  // recursion from the constant 1 and the function g : N2 −→ N given by g(n, m) = (n + 1) × m
  def g(n : Int, m : Int) : Int = (n+1) * m
  def h31(n : Int) : Int = if (n == 0) 1 else g(n-1, h31(n-1))

  // ***** Problem 3.2
  // Let h : N −→ N be the function defined by primitive recursion from the
  // constant 3 and the function g given by g(n, m) = 3n + 2m. We calculate the value of h(5).

  // define g(n, m) and then h in terms of h(n-1) and g as g(n-1, h(n-1))
  def g32(n : Int, m : Int) : Int = (3 * n) + (2 * m)
  def h321(n : Int) : Int = if (n == 0) 3 else g32(n-1, h321(n-1))

  // all in-one definition
  def h322(n : Int) : Int = if (n == 0) 3 else 3 * (n-1) + 2 * h322(n-1)


  // ***** Problem 3.3
  // Let h : N −→ N be defined by primitive recursion from the constant 5 and 
  // the function g : N2 −→ N given by g(n,m) = n2 + 1 + m. Calculate h(6).
  def h33(n : Int) : Int = if (n == 0) 5 else  (n-1) * (n-1) + 1 + h33(n-1)


  def main(args : Array[String]) {

    println("problem 3.1, answer h(5) = " + h31(5))

    println("problem 3.2 - 1, answer h(5) = " + h321(5))
    
    println("problem 3.2 - 2, answer h(5) = " + h322(5))

    println("problem 3.3, answer h(6) = " + h33(6))
  }
}

PrimitiveRecursion.main(args)

