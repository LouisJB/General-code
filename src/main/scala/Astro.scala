

// Astrophysics / cosmology / astronomy based functions
//   some simple stuff 
object Astro {

  import math._

  // basic universal constants
  object Consts {
    val c = 3E08 // m.s^-1
    val h = 6.63E-34 // J.s Plank
    val k = 1.38E-23 // J.K^-1 Boltzman
    val G = 6.67E-11 // N.m^2.kg^-2
    val sbSigma = 5.67E-8 // W.m^-2.K^-4 Stefan-Boltzmann
  }

  import Consts._

  // avoiding complexity of typed UOMs some simple type aliases for now
  type Kelvin = Double
  type Meter = Double
  type Joule = Double
  type Hz = Double
  type Kg = Double

  def lambda(f : Hz) : Meter = c / f
  def lPeak(t : Kelvin) : Meter = 2.9E-03 / t
  def energy(f: Hz) : Joule = h * f
  
  // radial velocity due to doppler shift (+ve = towards observer)
  def velr(lambdaObs : Meter, lambdaEm : Meter) = 
    c * (lambdaObs - lambdaEm) / lambdaEm

  // velocity dispursion due to velocities
  def velDispursion(deltaLambda : Meter, lambda : Meter) = 
    deltaLambda * c / lambda
  
  // velocity dispursion due to thermal broadening
  def thermalVelocityDispursion(temp : Kelvin, particalMass : Kg) = 
    sqrt(2.0 * k * temp / particalMass)

  // main-seq luminosity
  def L(radius : Meter, temp : Kelvin) = {
    val z = 4.0 * Pi * pow(radius, 2) * sbSigma * pow(temp, 4)
  }
}

