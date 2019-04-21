package uk.carwynellis.raytracing.montecarlo

import org.scalatest.{FlatSpec, Matchers}

class PiSpec extends FlatSpec with Matchers {

  behavior of "simpleEstimate"

  it should "compute an approximate estimate of Pi over 1000 iterations" in {
    Pi.simpleEstimate(1000) shouldBe 3.1 +- 0.2
  }

  it should "compute a better estimate of Pi over 100000 iterations" in {
    Pi.simpleEstimate(100000) shouldBe 3.14 +- 0.1
  }

  it should "compute a better estimate of Pi over 1000000 iterations" in {
    Pi.simpleEstimate(1000000) shouldBe 3.141 +- 0.01
  }

}
