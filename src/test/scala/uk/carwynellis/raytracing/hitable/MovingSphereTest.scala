package uk.carwynellis.raytracing.hitable

import org.scalatest.{FunSuite, Matchers}
import uk.carwynellis.raytracing.{HitRecord, Ray, Vec3}
import uk.carwynellis.raytracing.material.Dielectric

class MovingSphereTest extends FunSuite with Matchers {

  private val material = Dielectric(0.5)

  private val underTest = MovingSphere(
    centre0 = Vec3(3,3,3),
    centre1 = Vec3(4,4,4),
    radius = 1,
    material = Dielectric(0.5),
    time0 = 0,
    time1 = 1
  )

  test("should return None for ray that does not hit the sphere") {
    val r = Ray(Vec3(0,0,0), Vec3(-1,0,0), 0)

    underTest.hit(r, 0, 1) shouldBe None
  }

  test("should return a hit record for a ray that hits the sphere") {
    val r = Ray(Vec3(0,0,0), Vec3(3,3,3), 0)

    underTest.hit(r, 0, 1) shouldBe Some(HitRecord(
      0.8075499102701248,
      0.0,
      0.0,
      Vec3(2.4226497308103743,2.4226497308103743,2.4226497308103743),
      Vec3(-0.5773502691896257,-0.5773502691896257,-0.5773502691896257),
      material
    ))
  }

}
