package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing.hitable.Sphere
import uk.carwynellis.raytracing.texture.Texture
import uk.carwynellis.raytracing.{HitRecord, Ray}

case class IsoTropic(override val albedo: Texture) extends Material(albedo) {

  override def scatter(rayIn: Ray, record: HitRecord): Option[ScatterRecord] = {
    val scattered = Ray(record.p, Sphere.randomPointOnUnitSphere())
    val attenuation = albedo.value(record.u, record.v, record.p)
    Some(ScatterRecord(
      specularRay =  scattered,
      attenuation = attenuation,
      isSpecular = false,
      pdf = None
    ))
  }

}
