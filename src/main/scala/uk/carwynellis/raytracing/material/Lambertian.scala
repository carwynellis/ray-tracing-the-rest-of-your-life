package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing.hitable.Sphere
import uk.carwynellis.raytracing.texture.Texture
import uk.carwynellis.raytracing.{HitRecord, Ray}

class Lambertian(albedo: Texture) extends Material(albedo) {
  override def scatter(rayIn: Ray, record: HitRecord): Option[ScatterResult] = {
    val target = record.normal + Sphere.randomPointInUnitSphere()
    Some(ScatterResult(Ray(record.p, target, rayIn.time), albedo.value(record.u, record.v, record.p)))
  }

}

object Lambertian {
  def apply(albedo: Texture) = new Lambertian(albedo)
}