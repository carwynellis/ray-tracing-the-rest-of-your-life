package uk.carwynellis.raytracing.montecarlo

object Integral {

  /**
    * Compute the integral of a function f, from 0 to n, for a given number of samples.
    *
    * @param n
    * @param samples
    * @param f
    * @return
    */
  def integral(n: Int, samples: Int)(f: Double => Double) = {
    val average = (0 until samples).map { _ =>
      // Compute a random value on the line 0 to n
      val p = n * math.random()
      f(p)
    }.sum / samples

    average * n
  }

}
