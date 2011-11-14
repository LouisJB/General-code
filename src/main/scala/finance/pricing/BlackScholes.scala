package finance.pricing

import math._
import cern.jet.random.{Uniform, Normal}
import cern.jet.random.engine.MersenneTwister
import RandomVariables._

trait CallOrPut {
  def isInTheMoney(K : Double, F : Double) = intrinsic(K)(F) > 0
  def intrinsic(K : Double)(F : Double) : Double = money(K)(F) + stuff(K)(F)
  def money(K : Double)(F : Double) : Double
  def stuff(K : Double)(F : Double) : Double
}
case object Call extends CallOrPut{
  override def toString = "Call"
  def toChar = 'C'
  def money(K : Double)(F : Double) = if (F > K) -K else 0.0
  def stuff(K : Double)(F : Double) = if (F > K) F else 0.0
}
case object Put extends CallOrPut{
  override def toString = "Put"
  def toChar = 'P'
  def money(K : Double)(F : Double) = if (F < K) K else 0.0
  def stuff(K : Double)(F : Double) = if (F < K) -F else 0.0
}

object RandomVariables {
	def standardUniform() : Uniform = standardUniform(12345)
	def standardUniform(seed : Int) : Uniform = new Uniform(new MersenneTwister(seed))
	
  def normal(mean : Double, stDev : Double) : Normal = normal(mean, stDev, 12345)
  def normal(mean : Double, stDev : Double, seed : Int) : Normal = new Normal(mean, stDev, new MersenneTwister(seed))

  def standardNormal(seed : Int) : Normal = new Normal(0.0, 1.0, new MersenneTwister(seed))
}

object TestBS extends App {
  import BlackScholes._
  
  val values = (0 to 100 by 1).map(t => {
    (1.0 to 10.0 by 1.0).map(vol => {
      ("t = " + t/100.0 + ", vol = " + vol, ", price %.2f".format(undiscountedOptionPrice(F = 20.0, X = 10.0, callPut = Call, T = t/100.0, vol = vol)))
    })
  })
  
  println("call 1 = \n" + values.mkString("\n"))
}

// F = spot, X = strike
class BlackScholes(F : Double, X : Double, callPut : CallOrPut, T : Double, σ : Double) {
  require(T >= 0, "Negative time not allowed")
  require(σ >= 0, "Negative σ not allowed")

  private val isWorthIntrinsic = T == 0.0 || σ == 0.0
  val standardNormalInstance : Normal = standardNormal(12345)

  lazy val d1 = (log(F / X) + σ * σ * T / 2.0 ) / (σ * sqrt(T))
  lazy val d2 = d1 - σ * sqrt(T)

  lazy val (n1 : Double, n2 : Double) = if (isWorthIntrinsic){
    callPut match {
      case Call if Call.isInTheMoney(X, F) => {
        (1.0, 1.0)
      }
      case Call =>
      case Put if Put.isInTheMoney(X, F) =>
      case Put => (1.0, 1.0)
    }
  } else {
    (standardNormalInstance.cdf(d1).doubleValue(),
     standardNormalInstance.cdf(d2).doubleValue())
  }

  lazy val N1 : Double = n1
  lazy val N2 : Double = n2

  lazy val probabilityOfExercise = callPut match {
    case Call => N2
    case Put => 1 - N2
  }
  lazy val expectedPriceFractionGivenExercise = callPut match {
    case Call => N1
    case Put => 1 - N1
  }
  lazy val undiscountedOptionPrice = {
    callPut match {
      case Call => F * expectedPriceFractionGivenExercise - X * probabilityOfExercise
      case Put => -F * expectedPriceFractionGivenExercise + X * probabilityOfExercise
    }
  }
}

object BlackScholes {

  def undiscountedOptionPrice(F : Double, X : Double, callPut : CallOrPut, T : Double, vol : Double) : Double = {
    val bl = new BlackScholes(F, X, callPut, T, vol)
    bl.undiscountedOptionPrice
  }

  def value(callOrPut: CallOrPut, F: Double, K: Double, sigma: Double, r: Double, T: Double) = {
    undiscountedOptionPrice(F, K, callOrPut, T, sigma) * exp(-r * T)
  }
  
  def simpleDiscountedOptionPrice(F : Double, X : Double, callPut : CallOrPut, T : Double, vol : Double, R: Double) = {
    value(callPut, F * exp(R * T), X, vol, R, T)
  }
  
  override def toString = "Black Scholes"
}
