package uk.carwynellis.raytracing

import org.scalatest.{FunSuite, Matchers}
import uk.carwynellis.raytracing.hitable._
import uk.carwynellis.raytracing.material.{DiffuseLight, Lambertian}
import uk.carwynellis.raytracing.texture.ConstantTexture

class RendererTest extends FunSuite with Matchers {

  // Test that replicates the generation of component values exceeding 255.
  // This seems to be caused by having values in the light exceed 1
  // TODO - this doesn't seem to be accounted for in the code in the book - am I missing something here?
  test("renderer should not generate component values exceeding 255") {
    val width = 600
    val height = 400
    val samples = 4



    val failingScene = {
      val origin = Vec3(278, 278, -800)
      val target = Vec3(278, 278, 0)
      val time0 = 0.0
      val time1 = 0.2
      // Replicate scene generating bad values.
      val red = Lambertian(ConstantTexture(Vec3(0.65, 0.05, 0.05)))
      val light = DiffuseLight(ConstantTexture(Vec3(15, 15, 15)))
      val lightSource = XZRectangle(213, 343, 227, 332, 554, light)

      val objects = HitableList(List(
        YZRectangle(0, 555, 0, 555, 0, red), // Right wall
        lightSource
      ))

      Scene(
        objects = objects,
        raySources = HitableList(List(lightSource)),
        camera = Camera(
          origin = origin,
          target = target,
          upVector = Vec3(0, 1, 0),
          aspectRatio =  width.toDouble / height.toDouble,
          time0 = time0,
          time1 = time1
        )
      )

    }


    val renderer = Renderer(failingScene, width, height, samples)

    // This pixel is known to generate values out of bounds.
    val pixel = renderer.renderPixel(265, 347)

    pixel.r should be < 256.0
    pixel.g should be < 256.0
    pixel.b should be < 256.0
  }

}
