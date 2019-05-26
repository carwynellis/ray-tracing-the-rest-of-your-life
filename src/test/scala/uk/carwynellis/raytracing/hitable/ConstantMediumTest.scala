package uk.carwynellis.raytracing.hitable

import org.scalatest.{FunSuite, Matchers}
import uk.carwynellis.raytracing.material.{Dielectric, IsoTropic}
import uk.carwynellis.raytracing.texture.ConstantTexture
import uk.carwynellis.raytracing.{HitRecord, Ray, Vec3}

class ConstantMediumTest extends FunSuite with Matchers {

  private val boundary = Sphere(Vec3(0,2,3), 5, Dielectric(0.5))
  private val texture = ConstantTexture(Vec3(1,1,1))

  test("should return none for ray out of hitable bounds") {
    val r = Ray(Vec3(0,0,0), Vec3(1,0,0))
    val underTest = ConstantMedium(boundary, 1.0, texture, { () => 1.0 } )

    val result = underTest.hit(r, 0.0, 0.0)

    result shouldBe None
  }

  test("should return a hit record for a ray hitting the boundary") {
    val r = Ray(Vec3(0,0,0), Vec3(0,2,3))
    val underTest = ConstantMedium(boundary, 1.0, texture, { () => 1.0 } )

    val result = underTest.hit(r, 0.0, 1.0)

    result shouldBe Some(HitRecord(
      t = 0.0,
      u = 0.0,
      v = 0.0,
      p = Vec3(0,0,0),
      normal = Vec3(1,0,0),
      IsoTropic(texture)
    ))
  }

  test("should return none for a hit that is not inside the boundary") {
    val r = Ray(Vec3(0,0,0), Vec3(0,2,3))
    // Return a random double that ensures hitDistance > distanceInsideBoundary to trigger None return
    val underTest = ConstantMedium(boundary, 1.0, texture, { () => 0.01 } )

    val result = underTest.hit(r, 0.0, 1.0)

    result shouldBe None
  }

}
