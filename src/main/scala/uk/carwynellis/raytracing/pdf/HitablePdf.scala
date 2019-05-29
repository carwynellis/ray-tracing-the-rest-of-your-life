package uk.carwynellis.raytracing.pdf

import uk.carwynellis.raytracing.Vec3
import uk.carwynellis.raytracing.hitable.Hitable

class HitablePdf(p: Hitable, origin: Vec3) extends Pdf {

  override def value(direction: Vec3): Double = p.pdfValue(origin, direction)

  override def generate: Vec3 = p.random(origin)

}

object HitablePdf {
  def apply(p: Hitable, origin: Vec3) = new HitablePdf(p, origin)
}
