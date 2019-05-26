package uk.carwynellis.raytracing

import org.scalatest.{FunSuite, Matchers}

class AxisAlignedBoundingBoxTest extends FunSuite with Matchers {

  val underTest = AxisAlignedBoundingBox(
    min = Vec3(0, 0, 0),
    max = Vec3(2, 2, 2)
  )

  test("hit returns true for ray within bounding box") {
    val ray = Ray(
      origin = Vec3(0, 0, 0),
      direction = Vec3(1, 1, 1)
    )

    underTest.hit(ray, 0, 0.5) shouldBe true
  }

  test("hit returns false for ray outside bounding box") {
    val ray = Ray(
      origin = Vec3(-10, -10, -10),
      direction = Vec3(0, 0, 0)
    )

    underTest.hit(ray, 0, 0) shouldBe false
  }

}
