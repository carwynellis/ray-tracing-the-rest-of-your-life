package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing._
import uk.carwynellis.raytracing.material.Material

import scala.annotation.tailrec

class Sphere(val centre: Vec3, val radius: Double, val material: Material) extends Hitable {

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] =
    Sphere.hitAtCentre(centre, radius, material, r, tMin, tMax)

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = {
    val radiusVector = Vec3(radius, radius, radius)
    Some(AxisAlignedBoundingBox(
      min = centre - radiusVector,
      max = centre + radiusVector
    ))
  }

}

object Sphere {
  def apply(centre: Vec3, radius: Double, material: Material) = new Sphere(centre, radius, material)

  @tailrec
  def randomPointOnUnitSphere(): Vec3 = {
    val randomPoint = (2.0 * Vec3(
      x = Random.double,
      y = Random.double,
      z = Random.double
    )) - Vec3(1, 1, 1)
    if (randomPoint.squaredLength >= 1) randomPointOnUnitSphere()
    else randomPoint.unitVector
  }

  /**
    * To assist with image maps we need to compute a scaled image coordinate which can be used to map to a pixel on the
    * image.
    *
    * @param p
    * @return
    */
  def getSphereU(p: Vec3): Double = 1 - (Math.atan2(p.z, p.x) + math.Pi) / (2 * math.Pi)

  /**
    * To assist with image maps we need to compute a scaled image coordinate which can be used to map to a pixel on the
    * image.
    *
    * @param p
    * @return
    */
  def getSphereV(p: Vec3): Double = (Math.asin(p.y) + math.Pi/2) / math.Pi

  /**
    * Compute hit for a Sphere at a given centre.
    *
    * @param centre
    * @param radius
    * @param material
    * @param r
    * @param tMin
    * @param tMax
    * @return
    */
  def hitAtCentre(centre: Vec3,
                  radius: Double,
                  material: Material,
                  r: Ray,
                  tMin: Double,
                  tMax: Double): Option[HitRecord] = {

    def computeHit(t: Double) =
      if (t < tMax && t > tMin) {
        val pointAtParameter = r.pointAtParameter(t)
        val normal = (pointAtParameter - centre) / radius
        val record = HitRecord(
          t = t,
          u = if (material.computeTextureCoordinates) Sphere.getSphereU(normal) else 0,
          v = if (material.computeTextureCoordinates) Sphere.getSphereV(normal) else 0,
          p = pointAtParameter,
          normal = (pointAtParameter - centre) / radius,
          material = material
        )
        Some(record)
      }
      else None

    val oc = r.origin - centre

    val a = r.direction.dot(r.direction)
    val b = oc.dot(r.direction)
    val c = oc.dot(oc) - (radius * radius)

    val discriminant = Math.pow(b, 2) - (a * c)

    if (discriminant > 0) {
      val discriminantRoot = math.sqrt(discriminant)

      def calculateT(d: Double) = (-b + d) / a

      computeHit(calculateT(-discriminantRoot)) orElse
        computeHit(calculateT(discriminantRoot))
    }
    else None
  }

}

