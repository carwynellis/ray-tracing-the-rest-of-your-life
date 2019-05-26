package uk.carwynellis.raytracing

import scala.annotation.tailrec

/**
  * Defines an axis aligned bounding box (AABB) around a set of objects.
  *
  * Used to rapidly exclude rays that will never hit an object.
  *
  * @param min
  * @param max
  */
case class AxisAlignedBoundingBox(min: Vec3, max: Vec3) {

  /**
    * Determine whether a given ray has hit the bounding box or not.
    *
    * Note that this is a slightly faster algorithm offered as an alternative in the book and credited to Andrew Kensler
    * at Pixar.
    *
    * Originally this used tuples to handle the swapping but this resulted in a large nunmber of tuple instances
    * being created. I'm using vars here to handle the conditional swap.
    *
    * @param ray
    * @param tMin
    * @param tMax
    * @return
    */
  def hit(ray: Ray, tMin: Double, tMax: Double): Boolean = {

    var temp = 0.0
    var t0 = 0.0
    var t1 = 0.0

  @tailrec
    def loop(i: Int, lMin: Double, lMax: Double): Boolean = {
      val invD = 1.0 / ray.direction.get(i)

      t0 = (min.get(i) - ray.origin.get(i)) * invD
      t1 = (max.get(i) - ray.origin.get(i)) * invD

      if (invD < 0.0) {
        temp = t0
        t0 = t1
        t1 = temp
      }

      val sMin = if (t0 > lMin) t0 else lMin
      val sMax = if (t1 < lMax) t1 else lMax

      if (sMax <= sMin) false
      else if (i == 2) true
      else loop(i + 1, sMin, sMax)
    }

    loop(0, tMin, tMax)
  }

}

object AxisAlignedBoundingBox {
  def apply(min: Vec3, max: Vec3) = new AxisAlignedBoundingBox(min, max)

  def surroundingBox(box0: AxisAlignedBoundingBox, box1: AxisAlignedBoundingBox): AxisAlignedBoundingBox = {
    val min = Vec3(
      x = minD(box0.min.x, box1.min.x),
      y = minD(box0.min.y, box1.min.y),
      z = minD(box0.min.z, box1.min.z)
    )

    val max = Vec3(
      x = maxD(box0.max.x, box1.max.x),
      y = maxD(box0.max.y, box1.max.y),
      z = maxD(box0.max.z, box1.max.z)
    )

    AxisAlignedBoundingBox(min, max)
  }

  def minD(a: Double, b: Double): Double = if (a < b) a else b
  def maxD(a: Double, b: Double): Double = if (a > b) a else b
}
