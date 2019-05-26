package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing.material.Material
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray, Vec3}

class YZRectangle(val y0: Double,
                  y1: Double,
                  z0: Double,
                  z1: Double,
                  k: Double,
                  material: Material) extends Hitable {

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val t = (k - r.origin.x) / r.direction.x
    if (t < tMin || t > tMax) None
    else {
      val y = r.origin.y + (t * r.direction.y)
      val z = r.origin.z + (t * r.direction.z)
      if (y < y0 || y > y1 || z < z0 || z > z1) None
      else Some(HitRecord(
        t = t,
        u = (y - y0) / (y1 - y0),
        v = (z - z0) / (z1 - z0),
        p = r.pointAtParameter(t),
        normal = Vec3(1, 0, 0),
        material = material
      ))
    }
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = Some(AxisAlignedBoundingBox(
    min = Vec3(k - 0.0001, y0, z0),
    max = Vec3(k + 0.0001, y1, z1)
  ))

}


object YZRectangle {
  def apply(y0: Double, y1: Double, z0: Double, z1: Double, k: Double, material: Material) =
    new YZRectangle(y0, y1, z0, z1, k, material)
}


