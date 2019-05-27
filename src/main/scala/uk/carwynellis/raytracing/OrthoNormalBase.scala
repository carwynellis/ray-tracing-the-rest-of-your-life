package uk.carwynellis.raytracing

/**
  * Defines an Ortho-Normal Base composed of 3 mutually orthogonal vectors.
  *
  * This is used to allow random directions to be generated relative to an arbitrary point in space.
  *
  * @param u
  * @param v
  * @param w
  */
case class OrthoNormalBase(u: Vec3, v: Vec3, w: Vec3) {

  def local(a: Double, b: Double, c: Double): Vec3 = (a * u) + (b * v) + (c * w)

  def local(a: Vec3): Vec3 = (a.x * u) + (a.y * v) + (a.z * w)

}

object OrthoNormalBase {
  /**
    * Generate an OrthoNormalBase instance from a Vector which will be normalised and treated as the w component of the
    * OrthoNormalBase.
    *
    * The u and v components will be derived from this value.
    *
    * @param w Vector defining the w component of an OrthoNormalBase
    */
  def fromVectorAsW(w: Vec3) = {
    val wUnit = w.unitVector
    val a = if (Math.abs(wUnit.x) > 0.9) Vec3(0, 1, 0) else Vec3(1, 0, 0)
    val v = wUnit.cross(a).unitVector
    val u = w.cross(v)
    OrthoNormalBase(u, v, w)
  }
}
