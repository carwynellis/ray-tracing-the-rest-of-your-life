package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing.hitable.Sphere
import uk.carwynellis.raytracing.texture.Texture
import uk.carwynellis.raytracing.{HitRecord, Ray}

class Metal(albedo: Texture, fuzziness: Double) extends Material(albedo) {
  override def scatter(rayIn: Ray, record: HitRecord): Option[ScatterResult] = {
    val reflected = Material.reflect(rayIn.direction.unitVector, record.normal)
    Some(ScatterResult(Ray(record.p, reflected + (fuzziness * Sphere.randomPointInUnitSphere()), rayIn.time), albedo.value(0, 0, record.p)))
  }
}

object Metal {
  def apply(albedo: Texture, fuzziness: Double) = new Metal(albedo, fuzziness)
}
