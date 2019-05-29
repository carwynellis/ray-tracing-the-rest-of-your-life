package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing._

trait Hitable {

  def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord]

  def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox]

  def pdfValue(o: Vec3, v: Vec3): Double = 0

  def random(o: Vec3): Vec3 = Vec3(1, 0, 0)

}



