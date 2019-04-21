package uk.carwynellis.raytracing.montecarlo

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

}
