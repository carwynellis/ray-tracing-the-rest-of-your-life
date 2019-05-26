package uk.carwynellis.raytracing.hitable

import org.scalatest.{FunSuite, Matchers}
import uk.carwynellis.raytracing.material.Lambertian
import uk.carwynellis.raytracing.texture.ConstantTexture
import uk.carwynellis.raytracing.{HitRecord, Ray, Vec3}

class BoundingVolumeHierarchyTest extends FunSuite with Matchers {

  private val material = Lambertian(ConstantTexture(Vec3(1, 1, 1)))
  private val sphere = Sphere(Vec3(0,0,0), 1.0, material)
  private val anotherSphere = Sphere(Vec3(1,2,3), 5.0, material)

  test("should build a node with only the left child set to the single element for a list of a single hitable") {
    val hitables = List(sphere)

    val result = BoundingVolumeHierarchy.fromHitables(hitables, 0, 0)

    result.left shouldBe Some(sphere)
    result.right shouldBe None
  }

  test("should build a single node with left node set to first element and right node set to second for list of two hitables") {
    val hitables = List(sphere, anotherSphere)

    val result = BoundingVolumeHierarchy.fromHitables(hitables, 0, 0)

    result.left shouldBe Some(anotherSphere)
    result.right shouldBe Some(sphere)
  }

  test("should build a tree from a list of hitables") {
    val hitables = List(sphere, sphere, sphere)

    val expected = {
      val boundingBox = sphere.boundingBox(0, 0)

      val leftChild = BoundingVolumeHierarchy(Some(sphere), None, boundingBox, 0, 0)
      val rightChild = leftChild.copy(right = Some(sphere))

      BoundingVolumeHierarchy(
        left = Some(leftChild),
        right = Some(rightChild),
        box = boundingBox,
        time0 = 0,
        time1 = 0,
      )
    }

    BoundingVolumeHierarchy.fromHitables(hitables, 0, 0) shouldBe expected
  }

  test("hit should return a hit record for a ray that hits one of the spheres") {
    val hitables = List(sphere, sphere, sphere)

    val bvh = BoundingVolumeHierarchy.fromHitables(hitables, 0, 0)

    val r = Ray(Vec3(0,0,0), Vec3(3,3,3))

    bvh.hit(r, 0, 1) shouldBe Some(HitRecord(
      0.19245008972987526,
      0.0,
      0.0,
      Vec3(0.5773502691896257,0.5773502691896257,0.5773502691896257),
      Vec3(0.5773502691896257,0.5773502691896257,0.5773502691896257),
      material
    ))
  }

  test("hit should not return a hit record for a ray that misses one of the spheres") {
    val hitables = List(sphere, sphere)

    val bvh = BoundingVolumeHierarchy.fromHitables(hitables, 0, 0)

    val r = Ray(Vec3(8,8,8), Vec3(3,3,3))

    bvh.hit(r, 0, 1) shouldBe None
  }

}
