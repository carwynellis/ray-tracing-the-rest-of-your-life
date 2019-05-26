package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing._

trait Hitable {

  def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord]

  def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox]

}



