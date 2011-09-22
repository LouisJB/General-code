package finance

import math._

object Finance {
  import FxFuture._
  import Currency._

  def main(args : Array[String]) {

    val r1 = FxFuture(USD, YEN, 0.04, 0.06, 1.1).fwdFxRate

    println(r1)
  }
}

object Interest {

  def compound(i : Double, t : Int) = pow((1+i), t)
  def compound(i : Double, n : Int, t : Int) = pow((1+i/n), n*t)

  def toContinuous(y : Double) = pow(E, y)

  def toYield(r : Double) = log(r)
}

case class FxFuture(
  fromCurrency : Currency,
  toCurrency : Currency,
  fromYield : Double,
  toYield : Double,
  currentFx : Double) {

  def fwdFxRate = currentFx * ((1 + toYield)/(1 + fromYield))
}

trait Measure
trait Currency extends Measure
object Currency {
  case object Null extends Currency
  case object USD extends Currency
  case object GBP extends Currency
  case object YEN extends Currency
}

case class Quantity(value : Double, units : UOM = UOM.Null)
object Quantity {
  def apply() : Quantity = Quantity(0.0, UOM.Null)
}
case class Price(value : Double, currency : Currency)
object Price {
  def apply() : Price = Price(0.0, Currency.Null)
}
trait UOM
case class DefaultUOM(units : List[Measure], per : List[Measure]) extends UOM with Measure
object UOM {
  case object Null extends UOM
}
case object Meter extends Measure
case object Ton extends Measure
