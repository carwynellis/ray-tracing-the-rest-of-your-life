package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing._
import uk.carwynellis.raytracing.material.Material

/**
  * Class representing a moving sphere.
  *
  * For now this isn't a subclass of Sphere. If other objects are made 'moving' a general pattern might emerge that
  * could be expressed as a trait than through class hierarchies.
  *
  * @param centre0
  * @param centre1
  * @param radius
  * @param material
  * @param time0
  * @param time1
  */
class MovingSphere(val centre0: Vec3,
                   val centre1: Vec3,
                   val radius: Double,
                   val material: Material,
                   val time0: Double,
                   val time1: Double) extends Hitable {

  // Compute the location of the sphere centre at the specified time.
  private def centreAtTime(time: Double): Vec3 = centre0 + ( (time - time0) / (time1 - time0) ) * (centre1 - centre0)

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val centre = centreAtTime(r.time)
    Sphere.hitAtCentre(centre, radius, material, r, tMin, tMax)
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = {
    val radiusVector = Vec3(radius, radius, radius)

    def boundingBoxAtTime(t: Double) = AxisAlignedBoundingBox(
      min = centreAtTime(t) - radiusVector,
      max = centreAtTime(t) + radiusVector
    )

    Some(AxisAlignedBoundingBox.surroundingBox(
      box0 = boundingBoxAtTime(t0),
      box1 = boundingBoxAtTime(t1)
    ))
  }

}

object MovingSphere {
  def apply(centre0: Vec3, centre1: Vec3, radius: Double, material: Material, time0: Double, time1: Double) =
    new MovingSphere(centre0, centre1, radius, material, time0, time1)
}
