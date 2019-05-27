package uk.carwynellis.raytracing

import java.time.Duration

import uk.carwynellis.raytracing.hitable.Hitable
import uk.carwynellis.raytracing.material.ScatterResult

class Renderer(camera: Camera, scene: Hitable, width: Int, height: Int, samples: Int) {

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
    * @param depth
    * @return
    */
  def color(r: Ray, world: Hitable, depth: Int): Vec3 = {
    world.hit(r, ImageSmoothingLimit, Double.MaxValue) match {
      case Some(hit) =>
        val emitted = hit.material.emitted(hit.u, hit.v, hit.p)
        hit.material.scatter(r, hit) match {
          case Some(ScatterResult(_, attenuation, scatteredRay, pdf)) if depth < MaximumRecursionDepth =>
            // TODO - hardcoded values to validate calculations around sampling lights directly (Chapter 7)
            val onLight = Vec3(
              x = 213 + (Math.random() * (343-213)),
              y = 554,
              z = 227 + (math.random() * (332 - 227)),
            )
            val toLight = onLight - hit.p
            val distanceSquared = toLight.squaredLength
            val toLightUnit = toLight.unitVector

            if (toLightUnit.dot(hit.normal) < 0) emitted
            else {
              val lightArea = (343-213)*(332-227)
              val lightCosine = Math.abs(toLightUnit.y)
              if (lightCosine < 0.000001) emitted
              else {
                val pdf = distanceSquared / (lightCosine * lightArea)
                val scattered = Ray(hit.p, toLightUnit, r.time)

                emitted + attenuation * hit.material.scatterPdf(r, hit, scattered) * color(scattered, world, depth + 1) / pdf
              }
            }
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
    val result = (0 until samples).map { _ =>
      val xR = (x.toDouble + Random.double) / width.toDouble
      val yR = (y.toDouble + Random.double) / height.toDouble
      val ray = camera.getRay(xR, yR)
      color(ray, scene, 0)
    }.reduce(_ + _) / samples
    result.toPixel
  }

  /**
    * Renders the entire scene returning a list of Pixels representing the rendered scene.
    *
    * @return
    */
  def renderScene(): Seq[Pixel] = {
    val startTime = System.currentTimeMillis()
    (height-1 to 0 by -1).flatMap { j: Int =>
      showProgress(j, startTime)
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
      showProgress(pos, startTime)
      (0 until width).map(renderPixel(_, j))
    }.seq
  }

  // Basic progress indication, updated for each horizontal line of the image.
  // Added rather crude remaining time estimation which needs work.
  // TODO - this is scanline based so suffers when we encounter a more complex part of the scene. Consider rendering a
  //        whole scene sample and using this for the basis of parallelisation and estimation - should yield more
  //        accurate estimates.
  private def showProgress(hPos: Int, start: Long): Unit = {
    val durationSeconds = (System.currentTimeMillis() - start) / 1000
    val percentComplete = 100 - ((hPos.toDouble / height) * 100)
    val remainingDuration = {
      val durationUnit = durationSeconds / percentComplete
      Duration.ofSeconds((durationUnit * (100 - percentComplete)).toLong)
    }
    val elapsedDuration = Duration.ofSeconds(durationSeconds)
    printf(
      "\r% 4d%s complete - estimated time remaining %02d:%02d:%02d - elapsed %02d:%02d:%02d",
      percentComplete.toInt,
      "%",
      remainingDuration.toHours,
      remainingDuration.toMinutes % 60,
      remainingDuration.getSeconds % 60,
      elapsedDuration.toHours,
      elapsedDuration.toMinutes % 60,
      elapsedDuration.getSeconds % 60
    )
  }

}

object Renderer {
  def apply(camera: Camera, scene: Hitable, width: Int, height: Int, samples: Int) =
    new Renderer(camera, scene, width, height, samples)
}
