package uk.carwynellis.raytracing.texture
import uk.carwynellis.raytracing.Vec3

class NoiseTexture(scale: Double) extends Texture {

  private val perlinNoise = new Perlin

  override def value(u: Double, v: Double, p: Vec3): Vec3 =
    Vec3(1,1,1) * 0.5 * (1 + Math.sin(scale * p.x + 5 * perlinNoise.turbulence(scale * p)))
}

object NoiseTexture {
  def apply(scale: Double) = new NoiseTexture(scale)
}
