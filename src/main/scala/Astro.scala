// Astrophysics / cosmology / astronomy based functions
//   some simple stuff 
object Astro {

  import math._

  // avoiding complexity of typed UOMs some simple type aliases for now
  object Units {
    type Kelvin = Double
    type Meter = Double
    type Joule = Double
    type Hz = Double
    type Kg = Double
    type Watt = Double
  }

  // basic universal constants
  object Consts {
    import Units._
    val c = 3E08 // m.s^-1
    val Hzero = 72E3 / 3.09E16
    val h = 6.63E-34 // J.s Plank
    val k = 1.38E-23 // J.K^-1 Boltzman
    val G = 6.67E-11 // N.m^2.kg^-2
    val sbSigma = 5.67E-8 // W.m^-2.K^-4 Stefan-Boltzmann
    val AU : Meter = 1.5E11
    val pc : Meter = 3.09E16
    val eV : Joule = 1.60E-19
    val Lsun : Watt = 3.84E26
    val Tsun : Kelvin = 5770
  }

  import Units._
  import Consts._

  def lambda(f : Hz) : Meter = c / f
  def lPeak(t : Kelvin) : Meter = 2.9E-03 / t
  def energy(f : Hz) : Joule = h * f
    val sigma = 2

  // radial velocity due to doppler shift (+ve = towards observer)
  def velr(lambdaObs : Meter, lambdaEm : Meter) =
    c * (lambdaObs - lambdaEm) / lambdaEm

  // velocity dispursion due to velocities
  def velDispursion(deltaLambda : Meter, lambda : Meter) =
    deltaLambda * c / lambda

  // velocity dispursion due to thermal broadening
  def thermalVelocityDispursion(temp : Kelvin, particleMass : Kg) =
    sqrt(2.0 * k * temp / particleMass)

  // main-seq luminosity
  def luminosityByRadiusAndTemp(radius : Meter, temp : Kelvin) =
    4.0 * Pi * pow(radius, 2) * sbSigma * pow(temp, 4)

  def luminosity(distance : Meter, flux : Double) =
    4.0 * Pi * pow(distance, 2) * flux

  def M(apparentMag : Double, distance : Meter) =
    apparentMag - 5.0 * log(distance) + 5.0

  def P(rho : Double, temp : Kelvin, mass : Kg) =
    k * rho * temp / mass

  def schwarzschildRadius(mass : Kg) =
    (2.0 * G * mass) / pow(c, 2.0)

  // velocity of rotation around mass at radius
  def vel(mass : Kg, radius : Meter) =
    pow(G * mass / radius, 0.5)

  // red-shift
  def z(distance : Meter) =
    Hzero * distance / c

  // ref-shift
  def z(lambdaEm : Meter, lambdaObs : Meter) =
    (lambdaObs - lambdaEm) / lambdaEm
}
