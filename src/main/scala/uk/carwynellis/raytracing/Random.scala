package uk.carwynellis.raytracing

import java.util.SplittableRandom

/**
  * Wrapper around an instance of java.util.SplittableRandom which is reported to be
  * faster than math.random
  */
object Random {

  private val random = new SplittableRandom()

  def double: Double = random.nextDouble()

}
