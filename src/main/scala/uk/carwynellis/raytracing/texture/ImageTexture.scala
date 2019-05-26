package uk.carwynellis.raytracing.texture
import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import uk.carwynellis.raytracing.Vec3

import scala.util.{Failure, Success, Try}

class ImageTexture(image: BufferedImage) extends Texture {

  // Enable texture coordinates so we can map the image to the surface of the sphere.
  override def requiresTextureCoordinates: Boolean = true

  private val width = image.getWidth()
  private val height = image.getHeight()

  override def value(u: Double, v: Double, notUsed: Vec3): Vec3 = {
    val pixel = new Color(image.getRGB(u.toX, v.toY))

    Vec3(
      pixel.getRed / 255.0,
      pixel.getGreen / 255.0,
      pixel.getBlue / 255.0
    )
  }

  private implicit class DoubleOps(d: Double) {
    def toX: Int = toBoundedImageCoordinate(d, width)

    // Note - vertical direction of image inverted, e.g. 1 is top, 0 is bottom.
    def toY: Int = toBoundedImageCoordinate(1 - d, height)

    private def toBoundedImageCoordinate(c: Double, max: Int): Int = {
      val coord = c * max
      if (coord < 0) 0
      else if (coord > max - 1) max - 1
      else coord.toInt
    }
  }

}

object ImageTexture {

  // Run headless to prevent Boot process appearing and stealing focus.
  System.setProperty("java.awt.headless", "true")

  def fromPath(path: String): Either[Throwable, ImageTexture] = Try {
    new ImageTexture(ImageIO.read(new File(path)))
  } match {
    case Success(it) => Right(it)
    case Failure(ex) => Left(ex)
  }

  def apply(image: BufferedImage) = new ImageTexture(image)

}
