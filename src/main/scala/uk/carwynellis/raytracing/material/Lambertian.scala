package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing.hitable.Sphere
import uk.carwynellis.raytracing.texture.Texture
import uk.carwynellis.raytracing.{HitRecord, Ray}

class Lambertian(albedo: Texture) extends Material(albedo) {

  // TODO - review how scatter and scatterPdf interact - scope for improving this.
  override def scatter(rayIn: Ray, record: HitRecord): Option[ScatterResult] = {
    val target = record.p + record.normal + Sphere.randomPointOnUnitSphere()
    val scattered = Ray(record.p, (target - record.p).unitVector, rayIn.time)

    Some(ScatterResult(
      ray = Ray(record.p, target, rayIn.time),
      attenuation = albedo.value(record.u, record.v, record.p),
      scatteredRay = scattered,
      pdf = record.normal.dot(scattered.direction) / math.Pi
    ))
  }

  override def scatterPdf(rayIn: Ray, record: HitRecord, scattered: Ray): Double = {
    val cosine = {
      val c = record.normal.dot(scattered.direction.unitVector)
      if (c < 0) 0 else c
    }
    cosine / math.Pi
  }

}

object Lambertian {
  def apply(albedo: Texture) = new Lambertian(albedo)
}