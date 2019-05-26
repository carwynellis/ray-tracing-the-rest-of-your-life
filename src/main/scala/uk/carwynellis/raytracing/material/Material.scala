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
    * @param u
    * @param v
    * @param p
    * @return
    */
  def emitted(u: Double, v: Double, p: Vec3): Vec3 = Black

  def computeTextureCoordinates = albedo.requiresTextureCoordinates
}

object Material {

  def reflect(v: Vec3, n: Vec3): Vec3 = v - ( 2 * v.dot(n) * n)

}

case class ScatterResult(
  ray: Ray,
  attenuation: Vec3,
  // TODO - remove these defaults - just here to get up and running with the scatter changes in the book
  scatteredRay: Ray = Ray(Vec3(0, 0, 0), Vec3(0, 0, 0), 0.0),
  pdf: Double = 0.0
)



