package uk.carwynellis.raytracing.hitable.transform

import uk.carwynellis.raytracing.hitable.Hitable
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray, Vec3}

class Translate(p: Hitable, offset: Vec3) extends Hitable {

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val translatedRay = Ray(r.origin - offset, r.direction, r.time)
    p.hit(translatedRay, tMin, tMax).map(h => h.copy(p = h.p + offset))
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] =
    p.boundingBox(t0, t1).map(b => AxisAlignedBoundingBox(b.min, b.max + offset))

}

object Translate {
  def apply(p: Hitable, offset: Vec3) = new Translate(p, offset)
}
