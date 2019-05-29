package uk.carwynellis.raytracing.pdf

import uk.carwynellis.raytracing.material.Material
import uk.carwynellis.raytracing.{OrthoNormalBase, Vec3}

class CosinePdf(w: Vec3) extends Pdf {

  private val uvw = OrthoNormalBase.fromVectorAsW(w)

  override def value(direction: Vec3): Double = {
    val cosine = direction.unitVector.dot(uvw.w)
    if (cosine > 0) cosine / math.Pi else 0
  }

  // TODO - consider relocating randomCosineDirection
  override def generate: Vec3 = uvw.local(Material.randomCosineDirection)
}

object CosinePdf {
  def apply(w: Vec3) = new CosinePdf(w)
}
