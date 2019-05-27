package uk.carwynellis.raytracing.material

import uk.carwynellis.raytracing.{HitRecord, Ray, Vec3}
import uk.carwynellis.raytracing.texture.Texture

class DiffuseLight(emit: Texture) extends Material(emit) {

  override def scatter(rayIn: Ray, record: HitRecord): Option[ScatterResult] = None

  override def emitted(rayIn: Ray, record: HitRecord, u: Double, v: Double, p: Vec3): Vec3 =
    if (record.normal.dot(rayIn.direction) > 0) emit.value(u, v, p)
    else Vec3(0, 0, 0)

}

object DiffuseLight {
  def apply(emit: Texture) = new DiffuseLight(emit)
}

