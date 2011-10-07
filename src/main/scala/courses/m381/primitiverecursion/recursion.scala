package courses.m381.primitiverecursion

object recursion {

  // basic primitives
  def Z = 0
  def C0(n : Int) = 0
  def C1(n : Int) = 1
  def S(a : Int) = a + 1
  def succ(a : Int) = S(a)

  def U11(a : Int) = a
  def U21(a : Int, b : Int) = a
  def U22(a : Int, b : Int) = b
  def U31(a : Int, b : Int, c : Int) = a
  def U32(a : Int, b : Int, c : Int) = b
  def U33(a : Int, b : Int, c : Int) = c

  def sg(n : Int) = if (n > 0) 1 else 0
  def sgbar(n : Int) = if (n == 0) 1 else 0

  // characteristics
  def Xeq(n : Int,  m : Int) = sgbar(adf(n, m))

  def rem(n : Int, m : Int) : Int =
    if (n == 0) 0
    else
      mult((succ(rem(n-1, m))),
        mult(sg(m), (sgbar(Xeq(rem(n-1, m), cutOfSub(m, succ(Z)))))))

  
  //quot(0, m) = 0,
  //quot(n + 1, m) = sg(m)(quot(n, m) + sg(rem(n + 1, m)))
  def quot(n : Int, m : Int) : Int =
    if (n == 0) 0
    else 
      sg(m) * (quot(n-1, m) + sgbar(rem(n, m)))
  
  // total primitive recursive h : N2 => N
  def h(n1 : Int, n2 : Int)
        (f : Int => Int,
        g : (Int, Int, Int) => Int) : Int = {
    if (n2 == 0) f(n1)
    else g(n1, n2-1, h(n1, n2-1)(f, g))
  }
  
  def hh(n1 : Int, n2 : Int)
        (f : Int => Int,
        g : ((Int, Int, Int)) => Int) : Int = {
    if (n2 == 0) f(n1)
    else g((n1, n2-1, hh(n1, n2-1)(f, g)))
  }

  // derived functions...
  def mult(n : Int, m : Int) : Int = {
    def g(n1 : Int, n2 : Int, n3 : Int) : Int = add(U33(n1, n2, n3), U31(n1, n2, n3))
    if (m == 0) C0(n) else g(n, m-1, mult(n, m-1))
  }

  def exp2(n : Int, m : Int) : Int =
      hh(n, m)(
        C1 _,
        (a : (Int, Int, Int)) => mult(a._3, a._1))

  // f(n) = C1(n),
  // g(n, m, k) = mult(U3(n, m, k), U13(n, m, k))
  def exp(n : Int, m : Int) : Int = {
    def g(n1 : Int, n2 : Int, n3 : Int) : Int = mult(U33(n1, n2, n3), U31(n1, n2, n3))
    if (m == 0) C1(n) else g(n, m-1, mult(n, m-1))
  }

  def mult2(n : Int,  m : Int) : Int = {
    h(n,  m)(
      C0 _,
      (n1 : Int,  n2 : Int,  n3 : Int) => add(U33(n1, n2, n3), U31(n1, n2, n2)))
  }
  
  def mult3(n : Int,  m : Int) : Int = {
    hh(n,  m)(
      C0 _,
      (p : (Int, Int, Int)) => add(p._3, p._1))
  }
  
  // addition
  def add(n : Int, m : Int) : Int = {
    def g(a : Int, b  : Int, c : Int) = succ(U33(a, b, c))
    if (m == 0) U11(n) else g(n, m-1, add(n, m-1))
  }

  def add2(n : Int,  m : Int) : Int = {
    h(n, m)(
      U11 _,
      (n1 : Int,  n2 : Int,  n3 : Int) => succ(U33(n1, n2, n3)))
  }

  def cutOfSub(a : Int, b : Int) = if (a < b) 0 else a - b
  def max(a : Int, b : Int) = cutOfSub(a, b) + b
  def min(a : Int, b : Int) = cutOfSub(a, cutOfSub(a, b))
  def adf(a : Int, b : Int) = cutOfSub(a, b) + cutOfSub(b, a)
  
  def main(args : Array[String]) {

    println("add 3 + 4 = " + add(3, 4))
    println("add 3 + 4 = " + add2(3, 4))

    println("max 3, 4 = " + max(3, 4))
    println("max 4, 3 = " + max(4, 3))
    println("min 3, 4 = " + min(3, 4))
    println("min 4, 3 = " + min(4, 3))
    println("adf 3, 4 = " + adf(3, 4))
    println("adf 4, 3 = " + adf(4, 3))

    println("mult 3, 4 = " + mult(3, 4))
    println("mult 5, 4 = " + mult(5, 4))
    println("mult2 5, 4 = " + mult2(5, 9))
    println("mult3 5, 4 = " + mult3(5, 9))

    println("rem 12, 5 = " + rem(12, 5))
    println("rem 15, 4 = " + rem(15, 4))
    println("rem 6, 3 = " + rem(6, 3))

    println("exp 3, 4 = " + exp(3, 4))
    println("exp2 3, 4 = " + exp2(3, 4))

    println("quot 6, 3 = " + quot(6, 3))
    println("quot 13, 4 = " + quot(13, 4))
  }
}
