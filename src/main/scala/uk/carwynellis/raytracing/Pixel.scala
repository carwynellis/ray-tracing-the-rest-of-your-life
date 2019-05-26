package uk.carwynellis.raytracing

case class Pixel(r: Int, g: Int, b: Int)

object Pixel {

  // Maximum value per colour component.
  private val MaxComponentValue = 255.99

  def fromVec3(v: Vec3): Pixel = {
    new Pixel(
      r = v.x.componentValue,
      g = v.y.componentValue,
      b = v.z.componentValue
    )
  }

  private implicit class ComponentOps(val d: Double) extends AnyVal {
    // Gamma correct a component value using gamma2, e.g square root of the component value.
    def gammaCorrected: Double = clip(math.sqrt(d))

    // Compute the integer colour comopnent value e.g 1.0 would be 255
    def componentValue: Int = (d.gammaCorrected * MaxComponentValue).toInt

    // Prevent the computed value exceeding the maximum allowable value for the colour component.
    private def clip(d: Double) = if (d > 1) 1.0 else d
  }

}
