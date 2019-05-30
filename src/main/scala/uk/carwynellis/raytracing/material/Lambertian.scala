package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing.pdf.CosinePdf
import uk.carwynellis.raytracing.texture.Texture
import uk.carwynellis.raytracing.{HitRecord, Ray}

class Lambertian(albedo: Texture) extends Material(albedo) {

  override def scatter(rayIn: Ray, record: HitRecord): Option[ScatterRecord] = Some(ScatterRecord(
      specularRay = rayIn,
      isSpecular = false,
      attenuation = albedo.value(record.u, record.v, record.p),
      pdf = Some(CosinePdf(record.normal)),
    ))

  override def scatteringPdf(rayIn: Ray, record: HitRecord, scattered: Ray): Double = {
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