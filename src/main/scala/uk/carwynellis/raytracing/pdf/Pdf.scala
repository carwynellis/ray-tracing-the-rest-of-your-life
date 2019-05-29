package uk.carwynellis.raytracing.pdf

import uk.carwynellis.raytracing.Vec3

/**
  * Trait defining a common API for Probability Density Functions
  */
trait Pdf {

  def value(direction: Vec3): Double

  def generate: Vec3

}
