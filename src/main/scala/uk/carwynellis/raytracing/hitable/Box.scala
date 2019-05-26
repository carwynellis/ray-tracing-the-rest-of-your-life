package uk.carwynellis.raytracing.hitable
import uk.carwynellis.raytracing.material.Material
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray, Vec3}
import uk.carwynellis.raytracing.hitable.transform.FlipNormals.HitableToFlipNormalsOps

class Box(p0: Vec3, p1: Vec3, material: Material) extends Hitable {

  private val faces = HitableList(List(
    XYRectangle(p0.x, p1.x, p0.y, p1.y, p1.z, material),
    XYRectangle(p0.x, p1.x, p0.y, p1.y, p0.z, material).flipNormals,
    XZRectangle(p0.x, p1.x, p0.z, p1.z, p1.y, material),
    XZRectangle(p0.x, p1.x, p0.z, p1.z, p0.y, material).flipNormals,
    YZRectangle(p0.y, p1.y, p0.z, p1.z, p1.x, material),
    YZRectangle(p0.y, p1.y, p0.z, p1.z, p0.x, material).flipNormals,
  ))

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = faces.hit(r, tMin, tMax)

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] =
    Some(AxisAlignedBoundingBox(p0, p1))
}

object Box {
  def apply(p0: Vec3, p1: Vec3, material: Material) = new Box(p0, p1, material)
}
