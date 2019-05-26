package uk.carwynellis.raytracing.texture

import java.awt.Color
import java.awt.image.BufferedImage

import org.scalatest.{FunSuite, Matchers}
import uk.carwynellis.raytracing.Vec3

class ImageTextureTest extends FunSuite with Matchers {

  // Run headless to prevent Boot process appearing and stealing focus.
  System.setProperty("java.awt.headless", "true")


  // Create a four pixel test image, with each pixel a different colour.
  private val imageData = Map(
    (0,0) -> new Color(255, 0, 0).getRGB,
    (0,1) -> new Color(0, 255, 0).getRGB,
    (1,0) -> new Color(0, 0, 255).getRGB,
    (1,1) -> new Color(255, 255, 255).getRGB,
  )

  private val testImage = {
    val image = new BufferedImage(2, 2, BufferedImage.TYPE_INT_RGB)
    imageData.foreach { case ((x,y), color) => image.setRGB(x, y, color) }
    image
  }

  private val underTest = ImageTexture(testImage)

  // Note that the v component is effectively inverted, so 1 accesses the top of the image, 0 the bottom.

  test("imageTexture should return the correct colour for a coordinate within the image bounds") {
    underTest.value(0, 1, Vec3(0,0,0)) shouldBe Vec3(1, 0, 0)
  }

  test("imageText should enforce bounds for u value less than zero") {
    underTest.value(-100, 0, Vec3(0,0,0)) shouldBe Vec3(0, 1, 0)
  }

  test("imageText should enforce bounds for u value greater than one") {
    underTest.value(100, 0, Vec3(0,0,0)) shouldBe Vec3(1, 1, 1)
  }

  test("imageText should enforce bounds for v value less than zero") {
    underTest.value(0, -100, Vec3(0,0,0)) shouldBe Vec3(0, 1, 0)
  }

  test("imageText should enforce bounds for v value greater than one") {
    underTest.value(0, 100, Vec3(0,0,0)) shouldBe Vec3(1, 0, 0)
  }


}
