package uk.carwynellis.raytracing

object Render extends App {

  val width = 1200
  val height = 800
  val samples = 50

  val time0 = 0.0
  val time1 = 1.0

  val filename = "image.ppm"

  println(s"Rendering scene to $filename")

  val scene = Scene.cornellBoxScene
    .withStartTime(time0)
    .withEndTime(time1)
    .withAspectRatio(width.toDouble / height.toDouble)

  val renderer = Renderer(scene, width, height, samples)

  val imageWriter = ImageWriter(width, height, filename)

  renderer.renderScene().foreach(imageWriter.writePixel)

  imageWriter.close()

  println("\nFinished")
}
