package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing.material.Material
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray, Vec3}

class XZRectangle(val x0: Double,
                  x1: Double,
                  z0: Double,
                  z1: Double,
                  k: Double,
                  material: Material) extends Hitable {

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val t = (k - r.origin.y) / r.direction.y
    if (t < tMin || t> tMax) None
    else {
      val x = r.origin.x + (t * r.direction.x)
      val z = r.origin.z + (t * r.direction.z)
      if (x < x0 || x > x1 || z < z0 || z > z1) None
      else Some(HitRecord(
        t = t,
        u = (x - x0) / (x1 - x0),
        v = (z - z0) / (z1 - z0),
        p = r.pointAtParameter(t),
        normal = Vec3(0, 1, 0),
        material = material
      ))
    }
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = Some(AxisAlignedBoundingBox(
    min = Vec3(x0, k - 0.0001, z0),
    max = Vec3(x1, k + 0.0001, z1)
  ))

}


object XZRectangle {
  def apply(x0: Double, x1: Double, z0: Double, z1: Double, k: Double, material: Material) =
    new XZRectangle(x0, x1, z0, z1, k, material)
}


