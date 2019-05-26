package uk.carwynellis.raytracing.texture

import uk.carwynellis.raytracing.{Random, Vec3}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class Perlin {

  private val xPermutations = generateIndexPermutation()
  private val yPermutations = generateIndexPermutation()
  private val zPermutations = generateIndexPermutation()

  private val randomValues = generateNoise()

  private val indices = for {
    i <- 0 until 2
    j <- 0 until 2
    k <- 0 until 2
  } yield (i, j, k)

  def noise(p: Vec3): Double = {
    val u = p.x - Math.floor(p.x)
    val v = p.y - Math.floor(p.y)
    val w = p.z - Math.floor(p.z)


    val i = Math.floor(p.x).toInt
    val j = Math.floor(p.y).toInt
    val k = Math.floor(p.z).toInt

    val c = ArrayBuffer.fill(2, 2, 2)(Vec3(0, 0, 0))

    indices.foreach { case (di, dj, dk) =>
      c(di)(dj)(dk) =
        randomValues(xPermutations((i + di) & 255) ^ yPermutations((j + dj) & 255) ^ zPermutations((k + dk) & 255))
    }

    triLinearInterpolation(c, u, v, w)
  }

  def turbulence(p: Vec3, iterations: Int = 7): Double = {
    @tailrec
    def loop(acc: Double, weight: Double, q: Vec3, count: Int): Double = {
      if (count >= iterations) Math.abs(acc)
      else loop(acc + weight * noise(q), weight * 0.5, q * 2, count + 1)
    }

    loop(0.0, 1.0, p, 0)
  }

  private def generateNoise() =
    Seq.fill(256)(0.0).map(_ => Vec3(-1 + 2 * Random.double, -1 + 2 * Random.double, -1 + 2 * Random.double).unitVector)

  private def generateIndexPermutation(): IndexedSeq[Int] = util.Random.shuffle((0 until 256).toList).toIndexedSeq

  private def triLinearInterpolation(c: IndexedSeq[IndexedSeq[IndexedSeq[Vec3]]],
                                     u: Double, v: Double, w: Double) = {

    var accumulator = 0.0

    // Use hermite cubic to smooth interpolation results.
    val uu = u * u * (3 - 2 * u)
    val vv = v * v * (3 - 2 * v)
    val ww = w * w * (3 - 2 * w)

    indices.foreach { case (i, j, k) =>
      val weight = Vec3(u-i, v-j, w-k)
      accumulator = accumulator +
        ((i * uu + ((1 - i) * (1 - uu))) *
        (j * vv + ((1 - j) * (1 - vv))) *
        (k * ww + ((1 - k) * (1 - ww))) *
        c(i)(j)(k).dot(weight))
    }

    accumulator
  }

}
