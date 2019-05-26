package uk.carwynellis.raytracing.texture

import uk.carwynellis.raytracing.Vec3

trait Texture {

  def value(u: Double, v: Double, p: Vec3): Vec3

  /**
    * Set to true if texture coordinates are required. This is need for example for image maps.
    *
    * @return
    */
  def requiresTextureCoordinates = false

}
