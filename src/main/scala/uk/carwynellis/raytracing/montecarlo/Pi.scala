package uk.carwynellis.raytracing.montecarlo

import scala.collection.immutable

object Pi {

  /**
    * Compute estimate of Pi by selecting random points in a circle bounded by a square.
    *
    * The proportion of points within the circle should be proportional to the area of the circle.
    *
    * For Circle of radius R
    *
    * Proportion = (Pi*R*R)/(2R)*(2R) = Pi/4
    *
    * We use this to compute an estimate of Pi for a given number of iterations with a circle of radius 1, giving a
    * bounding square side length of 2.
    *
    * @param iterations
    * @return
    */
  def simpleEstimate(iterations: Int): Double = {

    def randomComponent = 2 * math.random() - 1

    val hitsInsideCircle = (1 to iterations).fold(0) { (acc: Int, _: Int) =>
      val x = randomComponent
      val y = randomComponent

      val insideCircle = ((x * x) + (y * y)) < 1

      if (insideCircle) acc + 1 else acc
    }

    (4.0 * hitsInsideCircle) / iterations
  }

  /**
    * Compute an estimate of Pi by computing a random point within each square of a grid.
    *
    * This approach is known as jittering and yields a more accurate result over fewer iterations.
    *
    * @param gridSize
    * @return
    */
  def stratifiedEstimate(gridSize: Int): Double = {

    def component(n: Int): Double = 2 * ((n + math.random()) / gridSize) - 1

    val hits = for {
      i <- 0 until gridSize
      j <- 0 until gridSize
    } yield {
      val x = component(i)
      val y = component(j)

      // Determine if point (x,y) is inside the circle or not.
      ((x * x) + (y * y)) < 1
    }

    val hitsInsideCircle = hits.count(h => h)

    (4.0 * hitsInsideCircle) / (gridSize * gridSize)
  }

}
