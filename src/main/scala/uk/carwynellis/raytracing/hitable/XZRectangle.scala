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

  override def pdfValue(o: Vec3, v: Vec3): Double = {
    hit(Ray(o, v), 0.001, Double.MaxValue).map { h =>
      val area = (x1 - x0) * (z1 - z0)
      val distanceSquared = h.t * h.t * v.squaredLength
      val cosine = math.abs(v.dot(h.normal) / v.length)
      distanceSquared / (cosine * area)
    }.getOrElse(0)
  }

  override def random(o: Vec3): Vec3 = {
    val randomPoint = Vec3(
      x = x0 + math.random() * (x1-x0),
      y = k,
      z = z0 + math.random() * (z1-z0),
    )

    randomPoint - o
  }
}


object XZRectangle {
  def apply(x0: Double, x1: Double, z0: Double, z1: Double, k: Double, material: Material) =
    new XZRectangle(x0, x1, z0, z1, k, material)
}


