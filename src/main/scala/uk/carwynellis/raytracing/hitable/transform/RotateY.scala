package uk.carwynellis.raytracing.hitable.transform

import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray, Vec3}
import uk.carwynellis.raytracing.hitable.Hitable

class RotateY(p: Hitable, angle: Double) extends Hitable {

  private val angleRadians = (math.Pi / 180) * angle

  private val sinTheta = Math.sin(angleRadians)
  private val cosTheta = Math.cos(angleRadians)

  private val boundingBox = p.boundingBox(0, 1).map { box =>
    val bounds = for {
      i <- 0 until 2
      j <- 0 until 2
      k <- 0 until 2

      x = i * box.max.x + (1 - i) * box.min.x
      y = j * box.max.y + (1 - j) * box.min.y
      z = k * box.max.z + (1 - k) * box.min.z

      newX = cosTheta * x + sinTheta * z
      newZ = -sinTheta * x + cosTheta * z

      test = Vec3(newX, y, newZ)
    } yield test

    def findMinValues(a: Vec3, b: Vec3): Vec3 = {
      val vecs = Seq(a, b)
      Vec3(
        x = vecs.map(_.x).min,
        y = vecs.map(_.y).min,
        z = vecs.map(_.z).min,
      )
    }

    def findMaxValues(a: Vec3, b: Vec3): Vec3 = {
      val vecs = Seq(a, b)
      Vec3(
        x = vecs.map(_.x).max,
        y = vecs.map(_.y).max,
        z = vecs.map(_.z).max,
      )
    }

    val min = bounds.fold(Vec3(Double.MaxValue, Double.MaxValue, Double.MaxValue)) { (a, b) => findMinValues(a, b) }
    val max = bounds.fold(Vec3(Double.MinValue, Double.MinValue, Double.MinValue)) { (a, b) => findMaxValues(a, b) }

    AxisAlignedBoundingBox(min, max)
  }

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val origin = r.origin.copy(
      x = cosTheta * r.origin.x - sinTheta * r.origin.z,
      z = sinTheta * r.origin.x + cosTheta * r.origin.z
    )
    val direction = r.direction.copy(
      x = cosTheta * r.direction.x - sinTheta * r.direction.z,
      z = sinTheta * r.direction.x + cosTheta * r.direction.z
    )

    val rotatedRay = Ray(origin, direction, r.time)

    p.hit(rotatedRay, tMin, tMax) map { hit =>
      val p = hit.p.copy(
        x = cosTheta * hit.p.x + sinTheta * hit.p.z,
        z = -sinTheta * hit.p.x + cosTheta * hit.p.z
      )
      val normal = hit.normal.copy(
        x = cosTheta * hit.normal.x + sinTheta * hit.normal.z,
        z = -sinTheta * hit.normal.x + cosTheta * hit.normal.z
      )
      hit.copy(p = p, normal = normal)
    }
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = boundingBox
}

object RotateY {
  def apply(p: Hitable, angle: Double) = new RotateY(p, angle)
}
