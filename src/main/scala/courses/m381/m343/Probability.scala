package courses.m381.m343

import scala.{Int, PartialFunction}
import math._

object Probability extends App {
  import math._
  type ~>[T, R] = PartialFunction[T, R]

  // simple utils/pimps
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
   * Poisson(μ) distributions
   * mean μ = λ
   * B(n, p) ~= Poission(np) for large n and small p
   */
  def poisson(μ : Double) : ~>[Int, Double] = {
    case z if z >= 0 => ((E pow (-1.0 * μ)) * (μ pow z)) / (z!)
  }

  // cdf (P(X <= x)
  def poissonCdf(μ : Double) : ~>[Int, Double] = {
    case k if k >= 0 =>
      ((E pow (-1.0 * μ)) * (0 to k).map(i => ((μ pow i) / (i!))).sum) //(0 until k).map(poisson(u)).sum
  }
  // 1 - cdf
  def poissonInvCdf(μ : Double) : ~>[Int, Double] = {
    case k if k >= 0 => 1 - poissonCdf(μ)(k)
  }

  def cdfPoisonIntegralTransform(μ : Double) : ~>[Double, Int] = {
    case u if ((u >= 0.0) && (u <= 1.0)) => ((0 to 1000000).find(x => poissonCdf(μ)(x) > u)).get
  }

  println("Poisson X ~ Poisson(1.8) P(X = 4) = " + poisson(1.8)(4))
  println("Poisson Y ~ Poisson(4), P(X>=3) = " + poissonInvCdf(4)(3))


  /**
   * Uniform distribution
   */
  def uniformPdf(a : Double, b : Double) : ~>[Double, Double] = {
    case x if a <= x && x <= b => 1.0 / (b - a)
  }
  
  /**
   * exponential X ∼ M(λ), f(x) = λe^(−λx), x >= 0
   * 
   */
  def exponentialPdf(λ : Double) : ~>[Double, Double] = {
    case x if x >= 0 => λ * exp(-1.0 * λ * x)
  }

  def exponentialCdf(λ : Double)(x : Double) = 1.0 - exp(-1.0 * λ * x)
 
  def meanExponential(λ : Double) = 1.0 / λ
  def varienceExponential(λ : Double) = 1.0 / (λ*λ)
  
  def centralLimitMean(μ : Double)(σ : Double)(n : Int) = Npdf(μ)(σ*σ/n)


  /**
   * Gamma Y ∼ Γ(n, λ),
   */
  def Γpdf(n : Int)(λ : Double) : ~>[Double, Double] = {
    case y if y >= 0 => (pow(y, n-1) * pow (λ, n) * exp(-1.0 * λ * y)) / ((n-1)!)
  }

  /**
   * Normal X ∼ N(μ, σ^2), f(x) ; x ∈ R
   */
  def Npdf(μ : Double)(σ : Double) : ~>[Double, Double] = {
    case x => 1.0 / (σ * sqrt(2.0 * Pi)) * exp(-(pow(x - μ, 2.0) / (2.0 * σ * σ)))
  }

  def NStdPdf : ~>[Double, Double] = {
    case x => 1.0 / (sqrt(2.0 * Pi)) * exp(-(pow(x, 2.0) / 2.0))
  }

  // some probability functions
  def notP(p : Double) = 1.0 - p
  def aIntersectB(pA : Double,  pB : Double,  pAUnionB : Double) = pA + pB - pAUnionB
  def conditionalAGivenE(pAUnionE : Double,  pE : Double) = pAUnionE / pE
  def bayesPAGivenB(pA : Double,  pB : Double, pBGivenA : Double) = (pBGivenA * pA) / pB
}

object TestProbability {
  import courses.m381.m343.Probability._

  def main(args : Array[String]) {

    val l = 14
    val λ = 14
    val μ = (λ * .5 * .4)
    println("λ = " + λ + ", " + μ)
    val p = poisson(μ)

    val f = poissonInvCdf(μ)
    val x1 = f(2)

    val f2 = poissonCdf(μ)
    val x2 = 1 - f2(2)

    val x3 = 1 - ((0 to 2).map(p).sum)

    val x4 = 1.0 - ((E pow (-1.0 * μ)) * (0 to 2).map(i => ((μ pow i) / (i!))).sum)

    println("x1 = " + x1 + ", x2 = " + x2 + ", x3 = " + x3 + ", x4 = " + x4)

    (0 to 10).map(x => (x, p(x), f2(x))).foreach(println)

    val invUP = cdfPoisonIntegralTransform(μ)
    val n = invUP(0.3872)
    println("n = " + n)


    val size = cdfPoisonIntegralTransform(μ)(0.25727)
    println("size = " + size)
    val us = List((0.64334, 0.08691), (0.18912, 0.59396))
    val positions = scalePositions(0.5, 0.4)(us)
    println("positions:\n" + positions.mkString(", "))


    val ls = List(6,4,5,6,6, 6,8,6,7,8, 5,9,10,6,8, 4,5,8,9,7)
    val k = ls.size
    val M = ls.sum.toDouble
    val avg = M.toDouble / k
    val sd = ls.map(x => (x - avg) pow 2).sum
    val xSqr = ls.map(x => x * x).sum
    val sd2 = xSqr - ((M pow 2) / k)
    println("sum = " + M + ", sd = " + sd + ", sd2 = " + sd2)
    val t = k * sd / ls.sum
    println("t = " + t)
  }

  def tryInvTrans = {
    val invUP = cdfPoisonIntegralTransform(4)
    val n = invUP(0.1396)
    println("n = " + n)
  }

  def scalePositions(a : Double, b : Double)(u : List[(Double, Double)]) : List[(Double, Double)] = {
    (0 until u.size).map(n => (u(n)._1 * a, u(n)._2 * b)).toList
  }
}

