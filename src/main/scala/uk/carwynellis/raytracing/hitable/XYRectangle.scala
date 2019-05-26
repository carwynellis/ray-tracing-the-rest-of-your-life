package uk.carwynellis.raytracing.hitable
import uk.carwynellis.raytracing.material.Material
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray, Vec3}

class XYRectangle(x0: Double,
                  x1: Double,
                  y0: Double,
                  y1: Double,
                  k: Double,
                  material: Material) extends Hitable {

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val t = (k - r.origin.z) / r.direction.z
    if (t < tMin || t> tMax) None
    else {
      val x = r.origin.x + (t * r.direction.x)
      val y = r.origin.y + (t * r.direction.y)
      if (x < x0 || x > x1 || y < y0 || y > y1) None
      else Some(HitRecord(
        t = t,
        u = (x - x0) / (x1 - x0),
        v = (y - y0) / (y1 - y0),
        p = r.pointAtParameter(t),
        normal = Vec3(0, 0, 1),
        material = material
      ))
    }
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = Some(AxisAlignedBoundingBox(
    min = Vec3(x0, y0, k - 0.0001),
    max = Vec3(x1, y1, k + 0.0001)
  ))

}

object XYRectangle {
  def apply(x0: Double, x1: Double, y0: Double, y1: Double, k: Double, material: Material) =
    new XYRectangle(x0, x1, y0, y1, k, material)
}
