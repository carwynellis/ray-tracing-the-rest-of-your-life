package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing.hitable.Sphere
import uk.carwynellis.raytracing.texture.Texture
import uk.carwynellis.raytracing.{HitRecord, Ray}

class Metal(albedo: Texture, fuzziness: Double) extends Material(albedo) {
  override def scatter(rayIn: Ray, record: HitRecord): Option[ScatterRecord] = {
    val reflected = Material.reflect(rayIn.direction.unitVector, record.normal)
    Some(ScatterRecord(
      specularRay = Ray(record.p, reflected + (fuzziness * Sphere.randomPointOnUnitSphere())),
      isSpecular = true,
      attenuation = albedo.value(0, 0, record.p),
      pdf = None, // TODO - does a specular ray always mean no PDF?
    ))
  }
}

object Metal {
  def apply(albedo: Texture, fuzziness: Double) = new Metal(albedo, fuzziness)
}
