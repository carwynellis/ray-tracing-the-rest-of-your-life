package uk.carwynellis.raytracing.texture
import uk.carwynellis.raytracing.Vec3

/**
  * Procedural texture that generates a checkerboard pattern of the provided textures.
  *
  * Typically this will be used with two ConstantTextures although any texture can be used.
  *
  * @param odd
  * @param even
  */
class CheckerBoard(odd: Texture, even: Texture) extends Texture {

  /**
    * Generate a checkerboard pattern by exploting the fact that the sign of the sine and
    * cosine alternate in a regular way so by multiplying the sine in all three dimensions
    * we can use the sign of the result to generate a checkerboard pattern in three
    * dimensions.
    *
    * @param u
    * @param v
    * @param p
    * @return
    */
  override def value(u: Double, v: Double, p: Vec3): Vec3 = {
    val sines = Math.sin(10 * p.x) * Math.sin(10 * p.y) * Math.sin(10 * p.z)
    if (sines < 0) odd.value(u, v, p)
    else even.value(u, v, p)
  }

}

object CheckerBoard {
  def apply(odd: Texture, even: Texture) = new CheckerBoard(odd, even)
}
