package courses.m381.m343

import scala.PartialFunction

object Probability extends App {
  import math._
  type ~>[T, R] = PartialFunction[T, R]

  // utils/pimps
  case class RichInt(n : Int) {
    def ! = fact(n)
    def pow(p : Int) : Int = n match {
      case _ if p == 0 => 1
      case _ if p == 1 => n
      case _ => (2 to p).foldLeft(n)((a, b) => a * n)
    }
    def /(d : Int) = n.toDouble / d
  }
  
  case class RichDouble(d : Double) {
    def pow(p : Int) = math.pow(d, p.toDouble)
    def pow(p : Double) = math.pow(d, p)
  }

  implicit def toRichInt(n : Int) = RichInt(n)
  implicit def toRichDouble(d : Double) = RichDouble(d)

  def fact(n : Int) =  n match {
    case _ if n < 0 => throw new IllegalArgumentException("Factorial undefined for n < 0")
    case _ if n <= 1 => 1
    case _ => (2 to n).reduceLeft(_ * _)
  }

  println("fact 0..10 = " + (0 to 10).map(fact).mkString(", "))

  // uniform distributions
  def discreteUniformDistribution(n : Int)(x : Int) = x match {
    case _ if x < 1 || x > n => throw new IllegalArgumentException("Argunent out of range, should be between 1 to n")
    case _ => 1.0/n
  }
  
  def discreteUniformDistribPfn(n : Int) : ~>[Int, Double] = {
    case x if x >= 0 && x <= n => 1.0 / n
  }

  // bernoullis
  def bernoulliDistrib(p : Double)(x : Int) = x match {
    case _ if x < 0 || x > 1 => throw new IllegalArgumentException("Bernoulli distrib range (0..1)")
    case _ => (p pow x) * ((1-p) pow (1-x))
  }
  
  def bernoulliDistribPfn(p : Double) : ~>[Int, Double] = {
    case x if x == 0 || x == 1 => (p pow x) * ((1-p) pow (1-x))
  }
  
  // binomials
  def binomial(n : Int)(k : Int) =
    if (k > n) throw new IllegalArgumentException("k can not be greated then n") else (n!) / ((k!) * ((n-k)!))
  
  def binomialDistrib(n : Int)(p : Double)(y : Int) = 
    (binomial(n)(y)) * (p pow y) * ((1.0 - p) pow (n-y))
  
  def binomialDistribPfn(n : Int)(p : Double) : ~>[Int,  Double] = {
    case y if y >= 0 => (binomial(n)(y)) * (p pow y) * ((1.0 - p) pow (n-y))
  }
  def binomialDistribXltK(n : Int)(p : Double) : ~>[Int, Double] = {
    case k if k >= 0 => (0 until k).map(binomialDistribPfn(n)(p)).sum
  }
  def binomialDistribXgteK(n : Int)(p : Double) : ~>[Int, Double] = {
    case k if k >= 0 => (k to n).map(binomialDistribPfn(n)(p)).sum
  }

  def negativeBinomialDistrib(p : Double)(k : Int) : ~>[Int, Double] = {
    case x if x >= k => (binomial(x-1)(k-1)) * (p pow k) * ((1-p) pow (x-k))
  }
  
  println("binomial 10c0..10 = " + (0 to 10).map(binomial(10) _).mkString(", "))
  val b5_onethird = binomialDistrib(5)(1.0/3) _
  println("binomial distrib Y ~ B(5, 1/3), P(Y == 0..5 " + (0 to 5).map(b5_onethird))

  println("Binomial X ~ B(6, 0.6), P(X = 2) = " + binomialDistrib(6)(0.6)(2))
  println("Binomial Y ~ B(7, 0.2), P(Y < 3) = " + binomialDistribXltK(7)(0.2)(3))
  println("Binomial X ~ B(10, 0.9), P(Z > 8) = " + (binomialDistribXgteK(10)(0.9)(8) - binomialDistribPfn(10)(0.9)(8)))
  
  // geometrics
  val geometricG1Pmf : Double => ~>[Int, Double] = (p : Double) => {
    case x : Int if x > 0 => p * ((1-p) pow (x-1))
  }
  
  val geometricG1XgtK : Double => ~>[Int, Double] = (p : Double) => {
    case k if k > 0 => ((1-p) pow (k-1))
  }

  val geometricG0Pmf : Double => ~>[Int, Double] = (p : Double) => {
    case x if x >= 0 => ((1-p) * (p pow (x)))
  }

  /**
   * Poisson(u) distributions
   * mean = u
   * B(n, p) ~= Poission(np) for large n and small p
   */
  def poisson(u : Double) : ~>[Int, Double] = {
    case z if z >= 0 => ((E pow (-1.0 * u)) * (u pow z)) / (z!)
  }
  
  def poissonZltK(u : Double) : ~>[Int, Double] = {
    case k if k >= 0 => (0 until k).map(poisson(u)).sum 
  }
  def poissonZgteK(u : Double) : ~>[Int, Double] = {
    case k if k >= 0 => 1 - poissonZltK(u)(k) 
  }
  
  println("Poisson X ~ Poisson(1.8) P(X = 4) = " + poisson(1.8)(4))
  println("Poisson Y ~ Poisson(4), P(X>=3) = " + poissonZgteK(4)(3))
  
  def notP(p : Double) = 1.0 - p
  def aIntersectB(pA : Double,  pB : Double,  pAUnionB : Double) = pA + pB - pAUnionB
  def conditionalAGivenE(pAUnionE : Double,  pE : Double) = pAUnionE / pE
  def bayesPAGivenB(pA : Double,  pB : Double, pBGivenA : Double) = (pBGivenA * pA) / pB
}

class Probability {

}
