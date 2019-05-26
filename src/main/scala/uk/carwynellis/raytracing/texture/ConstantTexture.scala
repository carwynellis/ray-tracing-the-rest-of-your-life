package uk.carwynellis.raytracing.texture

import uk.carwynellis.raytracing.Vec3

class ConstantTexture(color: Vec3) extends Texture {

  override def value(u: Double, v: Double, p: Vec3): Vec3 = color

}

object ConstantTexture {
  def apply(color: Vec3) = new ConstantTexture(color)
}
