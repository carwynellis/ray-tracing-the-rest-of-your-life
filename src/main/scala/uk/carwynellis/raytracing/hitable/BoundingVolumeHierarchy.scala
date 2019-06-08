package uk.carwynellis.raytracing.hitable
import uk.carwynellis.raytracing.{AxisAlignedBoundingBox, HitRecord, Random, Ray}

case class BoundingVolumeHierarchy(
  left: Option[Hitable],
  right: Option[Hitable],
  box: Option[AxisAlignedBoundingBox],
  time0: Double,
  time1: Double
) extends Hitable {

  /**
    * Compute whether a ray hits any of the hitables within the BVH.
    *
    * @param ray
    * @param tMin
    * @param tMax
    * @return
    */
  override def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    box.flatMap { b =>
      if (b.hit(ray, tMin, tMax)) {
        val leftHit = left.flatMap(_.hit(ray, tMin,tMax))
        val rightHit = right.flatMap(_.hit(ray, tMin, tMax))

        (leftHit, rightHit) match {
          case (Some(lh), Some(rh)) => if (lh.t < rh.t) Some(lh) else Some(rh)
          case (lh, None) => lh
          case (None, rh) => rh
        }
      }
      else None
    }
  }

  override def boundingBox(t0: Double, t1: Double): Option[AxisAlignedBoundingBox] = box
}

object BoundingVolumeHierarchy {

  def fromHitables(hitables: List[Hitable], time0: Double, time1: Double): BoundingVolumeHierarchy = {
    // Randomly select the axis we will sort on when splitting the next group of hitables. The intent here is to try to
    // ensure construction of a more balanced BVH tree.
    val axis = (3 * Random.double).toInt

    val sortedHitables = hitables.sortWith(sortForAxis(axis))

    val (left: Option[Hitable], right: Option[Hitable]) = sortedHitables.size match {
      case 1 => (sortedHitables.headOption, None)
      case 2 => (sortedHitables.headOption, sortedHitables.lastOption)
      case n =>
        val (hitablesLeft, hitablesRight) = sortedHitables.splitAt(n / 2)
        val leftNode = fromHitables(hitablesLeft, time0, time1)
        val rightNode = fromHitables(hitablesRight, time0, time1)
        (Some(leftNode), Some(rightNode))
    }

    val boundingBox = (left.flatMap(_.boundingBox(time0, time1)), right.flatMap(_.boundingBox(time0, time1))) match {
      case (Some(lb), Some(rb)) => Some(AxisAlignedBoundingBox.surroundingBox(lb, rb))
      case (lb, None) => lb
      case (None, rb) => rb
      case _ => None
    }

    new BoundingVolumeHierarchy(left, right, boundingBox, time0, time1)
  }

  private def compareAxis(l: Hitable,
                          r: Hitable,
                          f: (AxisAlignedBoundingBox, AxisAlignedBoundingBox) => (Double, Double)): Boolean = {
    val result = for {
      bl <- l.boundingBox(0, 0)
      br <- r.boundingBox(0, 0)
      bounds <- Some(f(bl, br)) // We could define a lift here to pull the result up into an Option but it seems like overkill.
    } yield (bounds._1 - bounds._2) < 0.0

    result.fold(false)(b => b)
  }

  private val compareXAxis = (l: Hitable, r: Hitable) => compareAxis(l, r, { (bl, br) => (bl.min.x, br.min.x)} )
  private val compareYAxis = (l: Hitable, r: Hitable) => compareAxis(l, r, { (bl, br) => (bl.min.y, br.min.y)} )
  private val compareZAxis = (l: Hitable, r: Hitable) => compareAxis(l, r, { (bl, br) => (bl.min.z, br.min.z)} )

  // Sort methods for x,y,z axes addressable from 0 - 2. Allows random selection of sort method when building the
  // BoundingVolumeHierarchy tree.
  private val sortForAxis = IndexedSeq(compareXAxis, compareYAxis, compareZAxis)

}
