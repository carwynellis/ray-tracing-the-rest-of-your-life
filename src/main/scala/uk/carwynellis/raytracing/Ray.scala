package uk.carwynellis.raytracing

class Ray(val origin: Vec3, val direction: Vec3, val time: Double = 0.0) {

  def pointAtParameter(t: Double): Vec3 = origin + (t * direction)

}

object Ray {
  def apply(origin: Vec3, direction: Vec3) = new Ray(origin, direction)
  def apply(origin: Vec3, direction: Vec3, time: Double) = new Ray(origin, direction, time)
}
