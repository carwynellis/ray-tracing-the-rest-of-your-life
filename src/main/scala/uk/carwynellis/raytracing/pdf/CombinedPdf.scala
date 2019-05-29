package uk.carwynellis.raytracing.pdf

import uk.carwynellis.raytracing.Vec3

/**
  * Combines two PDF functions with a 50/50 weighting.
  *
  * @param p0
  * @param p1
  */
class CombinedPdf(p0: Pdf, p1: Pdf) extends Pdf {

  override def value(direction: Vec3): Double = (p0.value(direction) + p1.value(direction)) / 2

  override def generate: Vec3 = if (math.random() < 0.5) p0.generate else p1.generate

}

object CombinedPdf {
  def apply(p0: Pdf, p1: Pdf) = new CombinedPdf(p0, p1)
}
