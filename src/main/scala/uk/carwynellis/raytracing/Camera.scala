package uk.carwynellis.raytracing

import scala.annotation.tailrec

/**
  * Class representing a camera which defines the parameters used to render the scene.
  *
  * @param origin position of the camera
  * @param target the point at which the camera is directed
  * @param upVector the vertical up vector for the camera
  * @param verticalFieldOfView the vertical field of view expressed in degrees
  * @param aspectRatio the aspect ratio of the image
  * @param time0 start time for motion blur handling, analagous to shutter open time
  * @param time1 end time for motion blur handling, analagous to shutter close time
  */
case class Camera(
  origin: Vec3,
  target: Vec3,
  upVector: Vec3,
  verticalFieldOfView: Double,
  aspectRatio: Double,
  aperture: Double,
  focusDistance: Double,
  time0: Double,
  time1: Double
) {

  private val lensRadius = aperture / 2.0

  private val theta = verticalFieldOfView * (math.Pi/180.0)
  private val halfHeight = Math.tan(theta/2.0)
  private val halfWidth = aspectRatio * halfHeight

  private val w = (origin - target).unitVector
  private val u = upVector.cross(w).unitVector
  private val v = w.cross(u)

  private val lowerLeftCorner =
    origin - (halfWidth * focusDistance * u) - (halfHeight * focusDistance * v) - (focusDistance * w)

  private val horizontal = 2.0 * halfWidth * focusDistance * u
  private val vertical = 2.0 * halfHeight * focusDistance * v

  def getRay(s: Double, t: Double): Ray = {

    @tailrec
    def randomPointInUnitDisk(): Vec3 = {

      val randomPoint = (2.0 * Vec3(
        x = Random.double,
        y = Random.double,
        z = 0
      )) - Vec3(1, 1, 0)

      if (randomPoint.dot(randomPoint) >= 1.0) randomPointInUnitDisk()
      else randomPoint
    }

    val rd = lensRadius * randomPointInUnitDisk()
    val offset = (u * rd.x) + (v * rd.y)

    // Compute a random time between time0 and time1 for motion blur handling.
    val rayTime = time0 + (Random.double * (time1 - time0))

    Ray(
      origin = origin + offset,
      direction = lowerLeftCorner + (s * horizontal) + (t * vertical) - origin - offset,
      time = rayTime
    )
  }

}

object Camera {
  def apply(origin: Vec3,
            target: Vec3,
            upVector: Vec3,
            verticalFieldOfView: Double,
            aspectRatio: Double,
            aperture: Double,
            focusDistance: Double,
            time0: Double = 0.0,
            time1: Double = 0.0) =
    new Camera(origin, target, upVector, verticalFieldOfView, aspectRatio, aperture, focusDistance, time0, time1)
}
