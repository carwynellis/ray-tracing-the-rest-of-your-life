package uk.carwynellis.raytracing.hitable

import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Ray, Vec3}

import scala.annotation.tailrec

class HitableList(val hitables: List[Hitable]) extends Hitable {

  private val hitableCount = hitables.size
  private val indexedHitables = hitables.toIndexedSeq // TODO - change hitables to indexedSeq instead?

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {

    @tailrec
    def loop(hs: List[Hitable], closest: Double, hitAnything: Boolean, record: Option[HitRecord]): Option[HitRecord] = {
      hs match {
        case x :: xs =>
          val hitResult = x.hit(r, tMin, closest)
          hitResult match {
            case Some(updatedRecord) => loop(xs, updatedRecord.t, hitAnything = true, Some(updatedRecord))
            case None => loop(xs, closest, hitAnything = hitAnything, record)
          }
        case Nil => record
      }
    }

    loop(hitables, closest = tMax, hitAnything = false, None)
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = {

    // Try to replicate the loop from the C++ using Option instead of booleans.
    def loop(acc: Option[AxisAlignedBoundingBox], h: List[Hitable]): Option[AxisAlignedBoundingBox] = acc match {
      case None => None
      case Some(b) => h match {
        case x :: xs =>
          val newAcc = x.boundingBox(t0, t1).map(AxisAlignedBoundingBox.surroundingBox(b, _))
          loop(newAcc, xs)
        case Nil => acc
      }
    }

    val firstBoundingBox = hitables.headOption.flatMap(_.boundingBox(t0, t1))

    loop(firstBoundingBox, hitables.tail)
  }

  override def pdfValue(o: Vec3, v: Vec3): Double = hitables.map(_.pdfValue(o, v)).sum / hitableCount

  override def random(o: Vec3): Vec3 = {
    val index = (math.random() * hitableCount).toInt
    indexedHitables(index).random(o)
  }

}

object HitableList {
  def apply(hitables: List[Hitable]) = new HitableList(hitables)
}
