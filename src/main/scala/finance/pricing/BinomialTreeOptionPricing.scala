package finance.pricing

import math._

case class BinomialTreeOptionPricing(
  n : Int  // no of time steps (height of binomial tree)
) {
  import Utils._
  def callOptionPrice(
    s : Double,   // spot
    x : Double,   // strike
    t : Double,   // time
    r : Double,   // risk free rate
    σ : Double    // vol = sigma (std-dev)
  ) : Double = {

    // adjust parameters for step size
    val rn : Double = exp(r * (t / n)) - 1
    val sdn : Double = σ * sqrt(t / n)
    val u = exp(rn + sdn)
    val d = exp(rn - sdn)
    val q : Double = (1 + rn - d) / (u - d)

    var acc : Double = 0.0
    for(k <- 0 to n by 1) {
     val coeff : Double = binomialCoefficient(n, k)
      var z : Double = coeff * pow(q, k) * pow(1 - q, n - k) * (s * pow(u, k) * pow(d, n - k) - x)
      if (z > 0)
        acc = acc + z
    }
    acc / pow(1 + rn, n)
  }
}

object Utils {
  import Numeric.Implicits._
  def binomialCoefficient(n : Int, k : Int) = (1 to k).foldLeft(1)((s, e) => (s * (n - e + 1)) / e)
  def average[T : Numeric](ls : List[T]) : Double = ls.sum.toDouble / ls.size.toDouble
  def stdDev[T : Numeric](ls : List[T]) : Double = pow(ls.map(x => pow((x.toDouble - average(ls)), 2)).sum / ls.size, 0.5)
  def variance[T : Numeric](ls : List[T]) = ls.map(x => pow((x.toDouble - average(ls)), 2)).sum / ls.size
  def continuousInterest(r : Double, t : Double) = exp(r * t)
}
object BinomialTreeOptionPricing extends App {
  import BlackScholes._

  def formatParams(s : Double, x : Double, t : Double, r : Double, σ : Double) = "Running s2, s = %.2f, x = %.2f, t = %.2f, r = %.2f, σ = %.2f".format(s, x, t, r, σ)
  
  def s1() {
    val s = 100.0
    val x = 95.0
    val t = 0.25
    val r = 0.07
    val σ = 0.2
    println("\nRunning s1: " + formatParams(s, x, t, r, σ))

    // references
    // http://www.soarcorp.com/black_scholes_calculator.jsp
    // undiscounted option price is Call = 6.888
    // discounted option price is Call = 8.056 @ r = 0.07

    val uop = undiscountedOptionPrice(F = s, X = x * exp(-r * t), callPut = Call, T = t, vol = σ)
    val dop = value(Call, s, x, σ, r, t)
    println("BS callOptionPrice = " + uop)

    val btop1 = BinomialTreeOptionPricing(10)
    val p1 = btop1.callOptionPrice(s, x, t, r, σ)
    println("BOPM callOptionPrice = " + p1)
  }

  // example taken from Hull (8.0th ed, p254), 1 step binomial
  def s2() {
    val s = 20.0
    val x = 21.0
    val t = 0.25
    val r = 0.12
    val σ = .2 //stdDev(18.0 :: 22.0 :: Nil) / s
    println("\nRunning s2 with: " + formatParams(s, x, t, r, σ))

    import BlackScholes._
    val uop = undiscountedOptionPrice(F = s, X = x * exp(-r * t), callPut = Call, T = t, vol = σ)
    val dop = value(Call, s, x, σ, r, t)
    println("BS callOptionPrice = " + uop)
    
    val btop1 = BinomialTreeOptionPricing(25)
    val p1 = btop1.callOptionPrice(s, x, t, r, σ)
    println("BOPM callOptionPrice = " + p1)
  }

  s1()
  s2()
}
