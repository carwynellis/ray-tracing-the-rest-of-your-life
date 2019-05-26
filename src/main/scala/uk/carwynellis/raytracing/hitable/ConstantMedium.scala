package uk.carwynellis.raytracing.hitable
import uk.carwynellis.raytracing.material.IsoTropic
import uk.carwynellis.raytracing.texture.Texture
import uk.carwynellis.raytracing._

class ConstantMedium(boundary: Hitable, density: Double, albedo: Texture, randomDouble: () => Double) extends Hitable {

  private val phaseFunction = IsoTropic(albedo)

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] =
    boundary.hit(r, Double.MinValue, Double.MaxValue).flatMap { hit1 =>
      boundary.hit(r, hit1.t + 0.0001, Double.MaxValue).flatMap { hit2 =>
        val t1 = if (hit1.t < tMin) tMin else hit1.t
        val t2 = if (hit2.t > tMax) tMax else hit2.t
        if (t1 < t2) computeHit(t1, t2, r)
        else None
      }
    }

  private def computeHit(t1: Double, t2: Double, r: Ray) = {
    val boundedT1 = if (t1 < 0) 0 else t1
    val distanceInsideBoundary = (t2 - boundedT1) * r.direction.length
    val hitDistance = -(1/density) * Math.log(randomDouble())
    if (hitDistance < distanceInsideBoundary) {
      val hitT: Double = boundedT1 + hitDistance / r.direction.length
      Some(HitRecord(
        t = hitT,
        p = r.pointAtParameter(hitT),
        normal = ConstantMedium.Normal, // Arbitrary value
        material = phaseFunction,
        u = 0.0,
        v = 0.0
      ))
    }
    else None
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = boundary.boundingBox(t0, t1)
}

object ConstantMedium {

  def apply(boundary: Hitable, density: Double, albedo: Texture, randomDouble: () => Double = Random.double _) =
    new ConstantMedium(boundary, density, albedo, randomDouble)

  val Normal = Vec3(1, 0, 0)

}
