package uk.carwynellis.raytracing

/**
  * Partial implementation of the Vec3 class from chapter 2 of ray tracing in one weekend.
  *
  * May need further revisions in order to work correctly. I've tried to port the C++ code as accurately as I can.
  *
  * @param x
  * @param y
  * @param z
  */
case class Vec3(x: Double, y: Double, z: Double) {

  /**
    * Return the component associated with the specified index, e.g. 0 -> x, 1 -> y, 2 -> z
    *
    * An index out of bounds will trigger an IllegalArgumentException.
    *
    * Note - using a pattern match is negligibly slower than using if .. else however either is considerably faster
    *        than using a collection and accessing by index.
    *
    * @param i
    * @return
    */
  def get(i: Int) = i match {
    case 0 => x
    case 1 => y
    case 2 => z
    case n => throw new IllegalArgumentException(s"index $n is out of bounds 0-2")
  }

  /**
    * Return a Vec3 with the value at the specified index updated.
    *
    * @param i
    * @return
    */
  def set(i: Int)(n: Double) = i match {
    case 0 => this.copy(x = n)
    case 1 => this.copy(y = n)
    case 2 => this.copy(z = n)
    case n => throw new IllegalArgumentException(s"index $n is out of bounds 0-2")
  }

  // Alias the x, y, z values.
  val r: Double = x
  val g: Double = y
  val b: Double = z

  def unary_-(): Vec3 = Vec3( -x, -y, -z)
  def unary_+(): Vec3 = this

  /**
    * Compute the vector length defined as the square root of x^2 + y^2 + z^2^
    * @return
    */
  def length: Double  = math.sqrt(squaredLength)

  /**
    * Computes the sum of each of the x,y,z components squared.
    *
    * @return
    */
  def squaredLength: Double = Math.pow(x, 2) + Math.pow(y, 2) + Math.pow(z, 2)

  /**
    * Generates a new vector with total length 1 maintaining the direction of the original vector.
    *
    * Note - this returns a new instance of the vector rather than modifying in place as per the mutable c++
    *        implementation.
    */
  def unitVector: Vec3 = this / length

  def +(that: Vec3) = Vec3(
    x + that.x,
    y + that.y,
    z + that.z
  )

  def -(that: Vec3) = Vec3(
    x - that.x,
    y - that.y,
    z - that.z
  )

  def *(that: Vec3) = Vec3(
    x * that.x,
    y * that.y,
    z * that.z
  )

  def *(n: Double) = Vec3(n * x, n * y, n * z)

  def /(that: Vec3) = Vec3(
    x / that.x,
    y / that.y,
    z / that.z
  )

  def /(n: Double) = Vec3(x / n, y / n, z / n)

  /**
    * Computes the dot product of this vector and the specified vector.
    *
    * @param that
    * @return
    */
  def dot(that: Vec3): Double = (x * that.x) + (y * that.y) + (z * that.z)

  /**
    * Computes the cross product of this vector and the specified vector.
    *
    * Implementation verified with https://betterexplained.com/articles/cross-product/
    *
    * @param that
    * @return
    */
  def cross(that: Vec3): Vec3 = Vec3(
    x = (y * that.z) - (z * that.y),
    y = -((x * that.z) - (z * that.x)),
    z = (x * that.y) - (y * that.x)
  )
}

object Vec3 {

  implicit class DoubleWithVec3Ops(val d: Double) extends AnyVal {
    def *(v: Vec3): Vec3 = v * d
  }

  implicit class Vec3ToPixelOps(val v: Vec3) extends AnyVal {
    def toPixel = Pixel.fromVec3(v)
  }

}
