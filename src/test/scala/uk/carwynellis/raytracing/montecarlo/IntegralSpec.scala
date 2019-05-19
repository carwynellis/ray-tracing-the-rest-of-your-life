package uk.carwynellis.raytracing.montecarlo

import org.scalatest.{FlatSpec, Matchers}

class IntegralSpec extends FlatSpec with Matchers {

  behavior of "integral"

  it should "compute the correct value for a constant function" in {
    Integral.integral(10, 1000)(_ => 1) shouldBe 10.0
  }

  it should "compute an estimate of the of integral of f(x) = x * x" in {
    Integral.integral(2, 10000000)(x => x * x) shouldBe 8.0/3.0 +- 0.001
  }
}
