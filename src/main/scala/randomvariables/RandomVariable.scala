package randomvariables

import math._
import cern.jet.random.Normal
import cern.jet.random.engine.MersenneTwister
import java.lang.Object

// abstract random variable who's exact distribution is provided by implementation
trait RandomScalar {
  def nextValue : Double
}

// a random process that creates new versions of itself from one time step to the next
trait RandomVariable[T] {
  def nextValue : T
}

// general (Normal) random process with mean and std dev
case class MarkovStochasticProcess(
  value : Double,
  mean : Double,
  sd : Double,
  stepSize : Double,
  timeStep : Double,
  nrs : RandomScalar) extends RandomVariable[MarkovStochasticProcess] {

  val sdStep = sd * sqrt(stepSize)

  def nextValue = {
    val t = timeStep + stepSize
    val movement = nrs.nextValue * sdStep + mean
    //println("movement = " + movement)
    val v = value + movement

    copy(value = v, timeStep = t)
  }
}

object MarkovStochasticProcess {
  def apply(
    value : Double,
    mean : Double,
    sd : Double,
    stepSize : Double,
    timeStep : Double = 0.0) : MarkovStochasticProcess = {

      // using Colt Normal distribution
      val r1 = new Normal(0.0, 1.0, /* mean, sd, */ new MersenneTwister(System.currentTimeMillis().toInt))
      val n1 = new Object with RandomScalar {
        def nextValue = r1.nextDouble()
      }

      // using Java's built-in random Gaussian (Normal(0, 1))
      val r2 = new java.util.Random
      val n2 = new Object with RandomScalar {
        def nextValue = r2.nextGaussian() // * sd + mean
      }
    
      MarkovStochasticProcess(value, mean, sd, stepSize, timeStep, n1)
  }

  def main(args : Array[String]) {
    val r1 = MarkovStochasticProcess(0.0, 0.0, 1.0, 0.1)

    val value1 = (1 to 10).foldLeft(r1)((a : MarkovStochasticProcess, b : Int) => a.nextValue)

    println(value1)
    println

    val series1 = (1 to 10).foldLeft(List(r1))(
    (ls : List[MarkovStochasticProcess], b : Int) => ls.head.nextValue :: ls).reverse

    series1.foreach(println)
  }
}

// a markov process random variable with N(0, 1) distribution
trait WienerProcess extends RandomVariable[WienerProcess] {
  val value : Double
  val stepSize : Double
  val timeStep : Double
  val msp : MarkovStochasticProcess
}

case class WienerProcessImpl(
  value : Double,
  stepSize : Double,
  timeStep : Double,
  msp : MarkovStochasticProcess) extends WienerProcess {

  def nextValue : WienerProcess = {
    val v = msp.nextValue
    copy(
      value = v.value, timeStep = v.timeStep, msp = v)
  }
}

object WienerProcess {
  def apply(
    value : Double,
    stepSize : Double,
    timeStep : Double = 0) : WienerProcess = {
    
    val msp = MarkovStochasticProcess(value, 0.0, 1.0, stepSize, timeStep)
    WienerProcessImpl(value, stepSize, timeStep, msp)
  }

  def main(args : Array[String]) {
    val r1 = WienerProcess(0.0, 0.1)

    val value1 = (1 to 10).foldLeft(r1)((a : WienerProcess, b : Int) => a.nextValue)

    println(value1)
    println

    val series1 = (1 to 10).foldLeft(List(r1))(
      (ls : List[WienerProcess], b : Int) => ls.head.nextValue :: ls).reverse

    series1.foreach(println)
  }
}

// Generalised Wiener process with mean drift and standard-dev
case class GeneralisedWienerRandomVariable(
        value : Double,
        mean : Double,
        sd : Double,
        stepSize : Double,
        timeStep : Double,
        wp : WienerProcess) {
  // todo
}

// Random variable that follows an Ito process using functions of x and t for mean and standard-dev
case class ItoRandomVariable(
        value : Double,
        meanFn : (Double, Double) => Double,
        sdFn : (Double, Double) => Double,
        stepSize : Double,
        timeStep : Double,
        wp : WienerProcess) {

  def nextValue : ItoRandomVariable = {
    val nextWp = wp.nextValue
    val v = value + meanFn(value, stepSize) + nextWp.value * sdFn(value, stepSize)
      copy(
        value = v, timeStep = nextWp.timeStep, wp = nextWp)
    }
}

case class ItoStockPriceModelRandomVariable2(
        value : Double,
        mean : Double,
        sd : Double,
        stepSize : Double,
        timeStep : Double,
        model : ItoRandomVariable) extends RandomVariable[ItoStockPriceModelRandomVariable2] {

  def nextValue = {
    val m = model.nextValue
    copy(
      value = m.value, timeStep = m.timeStep, model = m)
  }
}

object ItoStockPriceModelRandomVariable2 {
  def apply(
    value : Double,
    mean : Double,
    sd : Double,
    stepSize : Double,
    timeStep : Double,
    wp : WienerProcess = null) : ItoStockPriceModelRandomVariable2 = {

    val w = Option(wp).getOrElse(WienerProcess(0.0, stepSize, timeStep))
    val meanFn = (v : Double,  t : Double) => mean * v * t
    val sdFn = (v : Double,  t : Double) => v * sqrt(stepSize) * sd
    val model = ItoRandomVariable(value, meanFn, sdFn, stepSize, timeStep, w)
    ItoStockPriceModelRandomVariable2(value, mean, sd, stepSize, timeStep, model)
  }
}

// discrete time Ito process model for stock prices
case class ItoStockPriceModelRandomVariable(
        value : Double,
        mean : Double,
        sd : Double,
        stepSize : Double,
        timeStep : Double,
        wp : WienerProcess) extends RandomVariable[ItoStockPriceModelRandomVariable] {

  val sdStep = sd * sqrt(stepSize)

  def nextValue = {
    val nextWp = wp.nextValue
    val v = value + (mean * stepSize * value) + (nextWp.value * sdStep * value)
    copy(
      value = v, timeStep = nextWp.timeStep, wp = nextWp)
  }
}

object ItoStockPriceModelRandomVariable {
  def apply(
    value : Double,
    mean : Double,
    sd : Double,
    stepSize : Double,
    timeStep : Double = 0) : ItoStockPriceModelRandomVariable = {

    val wp = WienerProcess(0.0, stepSize, timeStep)
    ItoStockPriceModelRandomVariable(value, mean, sd, stepSize, timeStep, wp)
  }
}

object RandomVariableTest {
  def main(args : Array[String]) {
    series3
  }

  def series1 = {
    val r1 = ItoStockPriceModelRandomVariable(0.0, 0.0, 10.0, 1.0, 0.1)

    val value1 = (1 to 10).foldLeft(r1)((a : ItoStockPriceModelRandomVariable, b : Int) => a.nextValue)

    println(value1)
    println

    val series1 = (1 to 10).foldLeft(List(r1))(
      (ls : List[ItoStockPriceModelRandomVariable], b : Int) => ls.head.nextValue :: ls).reverse

    series1.foreach(println)
  }

  def series2 = {
    val r2 = ItoStockPriceModelRandomVariable(100.0, 0.15, 0.3, 1.0 / 52)
    val series2 = (1 to 10).foldLeft(List(r2))(
      (ls : List[ItoStockPriceModelRandomVariable], b : Int) => ls.head.nextValue :: ls).reverse

    series2.foreach(println)
  }
  
  def series3 = {
    // mock Wiener process, random sample e as per example from Hull r8 p289
    case class WPM(value : Double,  stepSize : Double,  timeStep : Double, var pos : Int = 0)
        extends WienerProcess {
      val msp : MarkovStochasticProcess = null // not used
      val ls = List(0.52, 1.44, -0.86, 1.46, -.69, -0.74, 0.21, -1.10, 0.73, 1.16, 2.56)
      override def nextValue : WienerProcess = {
        val v = ls(pos)
        pos = pos + 1
        copy(value = v, timeStep = pos * stepSize)
      }
    }
    
    val mockedWp1 = WPM(0.0, 1.0/52, 0.0)

    val r1 = ItoStockPriceModelRandomVariable(100.0, 0.15, 0.3, 1.0 / 52, 0.0, mockedWp1)
    val series1 = (1 to 10).foldLeft(List(r1))(
      (ls : List[ItoStockPriceModelRandomVariable], b : Int) => ls.head.nextValue :: ls).reverse

    series1.foreach(println)

    println("--")

    val mockedWp2 = WPM(0.0, 1.0/52, 0.0)
    
    val r2 = ItoStockPriceModelRandomVariable2(100.0, 0.15, 0.3, 1.0 / 52, 0.0, mockedWp2)
    val series2 = (1 to 10).foldLeft(List(r2))(
      (ls : List[ItoStockPriceModelRandomVariable2], b : Int) => ls.head.nextValue :: ls).reverse

    series2.foreach(println)
  }
}
