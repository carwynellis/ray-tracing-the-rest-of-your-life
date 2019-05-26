package uk.carwynellis.raytracing.hitable.transform

import uk.carwynellis.raytracing.hitable.Hitable
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray}

class FlipNormals(hitable: Hitable) extends Hitable {

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] =
    hitable.hit(r, tMin, tMax).map(r => r.copy(normal = -r.normal))

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = hitable.boundingBox(t0, t1)

}

object FlipNormals {

  def apply(hitable: Hitable) = new FlipNormals(hitable)

  implicit class HitableToFlipNormalsOps(h: Hitable) {
    def flipNormals = FlipNormals(h)
  }

}
