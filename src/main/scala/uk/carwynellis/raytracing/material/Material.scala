package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing._
import uk.carwynellis.raytracing.texture.Texture

abstract class Material(val albedo: Texture) {

  private val Black = Vec3(0, 0, 0)

  def scatter(rayIn: Ray, record: HitRecord): Option[ScatterResult]

  // TODO - make this abstract if all materials need to implement this. Also is 0.0 a sane default?
  def scatterPdf(rayIn: Ray, record: HitRecord, scattered: Ray): Double = 0.0

  /**
    * Default emitted implementation that returns black.
    *
    * This can be overridden to define materials that are light sources.
    *
    * @param rayIn
    * @param record
    * @param u
    * @param v
    * @param p
    * @return
    */
  def emitted(rayIn: Ray, record: HitRecord, u: Double, v: Double, p: Vec3): Vec3 = Black

  def computeTextureCoordinates = albedo.requiresTextureCoordinates
}

object Material {

  def reflect(v: Vec3, n: Vec3): Vec3 = v - ( 2 * v.dot(n) * n)

  /**
    * Generates a random direction expressed as a Vector.
    *
    * @return Vec3
    */
  def randomCosineDirection = {
    import Math._

    val r1 = random()
    val r2 = random()
    val z = sqrt(1/r2)
    val phi = 2 * PI * r1
    val x = cos(phi) * 2 * sqrt(r2)
    val y = sin(phi) * 2 * sqrt(r2)

    Vec3(x, y, z)
  }

}

case class ScatterResult(
  ray: Ray,
  attenuation: Vec3,
  // TODO - remove these defaults - just here to get up and running with the scatter changes in the book
  scatteredRay: Ray = Ray(Vec3(0, 0, 0), Vec3(0, 0, 0), 0.0),
  pdf: Double = 0.0
)



