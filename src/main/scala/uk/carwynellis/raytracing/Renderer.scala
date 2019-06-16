package uk.carwynellis.raytracing

import uk.carwynellis.raytracing.hitable.Hitable
import uk.carwynellis.raytracing.material.ScatterRecord
import uk.carwynellis.raytracing.pdf.{CombinedPdf, HitablePdf}

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, MILLISECONDS}

class Renderer(
  scene: Scene,
  width: Int,
  height: Int,
  samples: Int
) {

  // When rendering some rays may may include a floating point error preventing them from being treated as 0.
  // We increase the minimum value we accept slightly which yields a smoother image without visible noise.
  val ImageSmoothingLimit = 0.001

  val BlackBackground = Vec3(0, 0, 0)

  val MaximumRecursionDepth = 50

  /**
    * Compute the color for a given ray.
    *
    * @param r
    * @param world
    * @param raySources
    * @param depth
    * @return
    */
  def color(
    r: Ray,
    world: Hitable,
    raySources: Hitable,
    depth: Int
  ): Vec3 = {
    world.hit(r, ImageSmoothingLimit, Double.MaxValue) match {
      case Some(hit) =>
        val emitted = hit.material.emitted(r, hit, hit.u, hit.v, hit.p)
        hit.material.scatter(r, hit) match {
          case Some(ScatterRecord(ray, true, attenuation, None)) if depth < MaximumRecursionDepth =>
            attenuation * color(ray, world, raySources, depth + 1)
          case Some(ScatterRecord(_, _, attenuation, Some(pdf))) if depth < MaximumRecursionDepth =>
            val lightPdf = HitablePdf(raySources, hit.p)
            val combinedPdf = CombinedPdf(lightPdf, pdf)
            val scattered = Ray(hit.p, combinedPdf.generate, r.time)
            val pdfValue = combinedPdf.value(scattered.direction)
            emitted + attenuation * hit.material.scatteringPdf(r, hit, scattered) * color(scattered, world, raySources, depth + 1) / pdfValue
          case _ => emitted
        }
      // If the ray hits nothing draw return black.
      case None => BlackBackground
    }

  }

  /**
    * Sample a number of randomly generated rays for the current pixel.
    *
    * Higher sample counts yield a better quality image at the expense of longer render times.
    * @param x
    * @param y
    * @return
    */
  def renderPixel(x: Int, y: Int): Pixel = {
    val result: Vec3 = (0 until samples).map { _ =>
      val xR = (x.toDouble + Random.double) / width.toDouble
      val yR = (y.toDouble + Random.double) / height.toDouble
      val ray = scene.camera.getRay(xR, yR)
      color(r = ray, world = scene.world, depth = 0, raySources = scene.raySources)
    }.reduce(_ + _) / samples
    result.toPixel
  }

  def renderPixelNoSample(x: Int, y: Int): Vec3 = {
    val xR = (x.toDouble + Random.double) / width.toDouble
    val yR = (y.toDouble + Random.double) / height.toDouble
    val ray = scene.camera.getRay(xR, yR)
    color(r = ray, world = scene.world, depth = 0, raySources = scene.raySources)
  }

  /**
    * Renders the entire scene returning a list of Pixels representing the rendered scene.
    *
    * @return
    */
  def renderScene(): Seq[Pixel] = {
    val startTime = System.currentTimeMillis()
    (height-1 to 0 by -1).flatMap { j: Int =>
      updateProgress((height - j) + 1, height, startTime)
      (0 until width).map(renderPixel(_, j))
    }
  }

  /**
    * Renders the entire scene returning a list of Pixels representing the rendered scene.
    *
    * Uses ParSeq to parallelize the render.
    *
    * @return
    */
  def renderScenePar(): Seq[Pixel] = {
    val startTime = System.currentTimeMillis()
    @volatile var pos = height - 1
    (height-1 to 0 by -1).par.flatMap { j: Int =>
      pos -= 1
      updateProgress((height - pos) + 1, height, startTime)
      (0 until width).map(renderPixel(_, j))
    }.seq
  }

  /**
    * Renders the scene by sampling the entire scene each time, unlike the scanline based approached used by the other
    * render methods.
    *
    * This has the advantage of generating a more accurate estimated time remaining as the render progresses.
    *
    * @return
    */
  def renderScenePerSceneSamples(): Seq[Pixel] = {
    @tailrec
    def loop(acc: IndexedSeq[Vec3], sampleIndex: Int = 1, startTime: Long = System.currentTimeMillis()): Seq[Vec3] = {
      if (sampleIndex > samples) acc
      else {
        val data = (height - 1 to 0 by -1).par.flatMap { j: Int =>
          val pos = height - j - 1
          (0 until width).map { i =>
            val index = pos +  (pos * (width - 1)) + i
            renderPixelNoSample(i, j) + acc(index)
          }
        }.toIndexedSeq

        updateProgress(sampleIndex, samples, startTime)

        loop(data, sampleIndex + 1, startTime)
      }
    }

    showInitialProgress()

    val data = loop(IndexedSeq.fill(width * height)(Vec3(0, 0, 0)))

    data.map(_ / samples).map(_.toPixel)
  }

  private def millisecondsToTimeStamp(ms: Long): String = {
    val d = Duration(ms, MILLISECONDS)
    f"${d.toHours}%02d:${d.toMinutes % 60}%02d:${d.toSeconds % 60}%02d"
  }

  private def updateProgress(unitsComplete: Int, totalUnits: Int, startTime: Long): Unit = {
    val elapsedTime = System.currentTimeMillis()  - startTime
    val timePerUnit = elapsedTime / unitsComplete
    val estimatedTimeRemaining = (totalUnits - unitsComplete) * timePerUnit
    val percentComplete = (unitsComplete.toDouble / totalUnits.toDouble) * 100

    printf("\r% 3d%s complete - estimated time remaining %s - elapsed time %s ",
      percentComplete.toInt,
      "%",
      millisecondsToTimeStamp(estimatedTimeRemaining),
      millisecondsToTimeStamp(elapsedTime)
    )
  }

  private def showInitialProgress() = updateProgress(1, Int.MaxValue, System.currentTimeMillis())

}

object Renderer {
  def apply(scene: Scene, width: Int, height: Int, samples: Int) =
    new Renderer(scene, width, height, samples)
}
