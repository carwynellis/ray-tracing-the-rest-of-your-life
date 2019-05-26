package uk.carwynellis.raytracing.hitable.transform

import org.scalatest.{FunSuite, Matchers}
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, Vec3}
import uk.carwynellis.raytracing.hitable.XYRectangle
import uk.carwynellis.raytracing.material.Dielectric

class RotateYTest extends FunSuite with Matchers {

  private val material = Dielectric(0.5)

  private val underTest = XYRectangle(0, 10, 0, 10, 1, material)

  test("should generate a valid AxisAlignedBoundingBox") {
    underTest.boundingBox(0, 1) shouldBe Some(AxisAlignedBoundingBox(Vec3(0.0,0.0,0.9999),Vec3(10.0,10.0,1.0001)))
  }

}
