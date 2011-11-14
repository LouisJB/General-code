package finance.pricing

import math._
import mathutils._

trait Direction
case object CallOption extends Direction
case object PutOption extends Direction

trait OptionType
case object AmericanOption extends OptionType
case object EuropeanOption extends OptionType
case object BermudanOption extends OptionType

case class BinomialTreeOptionPricing(
  n : Int  // no of time steps (height of binomial tree)
) {
  import Binomial._

  def optionPrice(
      d : Direction,
      ot : OptionType,  // optionType
      s : Double,       // spot
      x : Double,       // strike
      t : Double,       // time
      r : Double,       // risk free rate
      σ : Double        // vol = sigma (std-dev)
    ) : Double = {

    val p = Array.tabulate(n+1)(x => 0.0)
    val q = 0.0 // dividend rate
    val dT = t / n
    val up = exp(σ * sqrt(dT))

    val p0 = (up * exp(-r * dT) - exp(-q * dT)) * up / (pow(up, 2) - 1)
    val p1 = exp(-r * dT) - p0

    // initial values at time T
    for (i <- (0 to n)) {
      d match {
        case PutOption => {
          p(i) = max(x - s * pow(up, 2*i - n), 0)
        }
        case CallOption => {
          p(i) = max(s * pow(up, 2*i - n) - x, 0)
        }
      }
    }

    // move to earlier times
    for (j <- (n-1 to 0  by -1)) {
      for (i <- 0 to j) {
        p(i) = p0 * p(i) + p1 * p(i+1)  // binomial value
        ot match {
          case AmericanOption => {
            d match {
              case PutOption => {
                val exercise = x - s * pow(up, 2*i - j) // exercise value
                p(i) = max(exercise, p(i))
              }
              case CallOption => {
                val exercise = s * pow(up, 2*i - j) - x // exercise value
                p(i) = max(exercise, p(i))
              }
            }
          }
          case EuropeanOption =>
          case _ => throw new Exception("Unsupported option type")
        }
      }
    }
    p(0)
  }

  // suffers from rounding problems...
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
     val coeff : Double = C(n, k)
      var z : Double = coeff * pow(q, k) * pow(1 - q, n - k) * (s * pow(u, k) * pow(d, n - k) - x)
      if (z > 0)
        acc = acc + z
    }
    acc / pow(1 + rn, n)
  }
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
    //
    // undiscounted option price is Call = 6.888
    // discounted option price is Call = 8.056 @ r = 0.07
    //
    // http://www.intrepid.com/robertl/option-pricer1/option-pricer.cgi

    val ucop = undiscountedOptionPrice(F = s * exp(r * t), X = x, callPut = Call, T = t, vol = σ) * exp (-r * t)
    println("BS call option price = " + ucop)

    val upop = simpleDiscountedOptionPrice(F = s, X = x, callPut = Put, T = t, vol = σ, R = r)
    println("BS put option price = " + upop)

    val btop = BinomialTreeOptionPricing(1000)
    val ceop = btop.optionPrice(CallOption, EuropeanOption, s, x, t, r, σ)
    println("BOPM call european option price = " + ceop)

    val caop = btop.optionPrice(CallOption, AmericanOption, s, x, t, r, σ)
    println("BOPM call american option price = " + caop)

    val peop = btop.optionPrice(PutOption, EuropeanOption, s, x, t, r, σ)
    println("BOPM put european option price = " + peop)

    val paop = btop.optionPrice(PutOption, AmericanOption, s, x, t, r, σ)
    println("BOPM put american option price = " + paop)

    val btop2 = BinomialTreeOptionPricing(10)
    val p1 = btop2.callOptionPrice(s, x, t, r, σ)
    println("BOPM call option price = " + p1)
  }

  // example taken from Hull (8.0th ed, p254), 1 step binomial
  def s2() {
    val s = 20.0
    val x = 21.0
    val t = 0.25
    val r = 0.12
    val σ = 0.2 // stdDev(18.0 :: 22.0 :: Nil) / s
    println("\nRunning s2 with: " + formatParams(s, x, t, r, σ))

    import BlackScholes._
    val uop = undiscountedOptionPrice(F = s, X = x * exp(-r * t), callPut = Call, T = t, vol = σ)
    val dop = value(Call, s, x, σ, r, t)
    println("BS callOptionPrice = " + uop)
    
    val btop1 = BinomialTreeOptionPricing(5)
    val p1 = btop1.callOptionPrice(s, x, t, r, σ)
    println("BOPM callOptionPrice = " + p1)
  }

  s1()
  s2()
}
