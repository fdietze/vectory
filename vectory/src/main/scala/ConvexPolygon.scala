package vectory

import annotation.meta.field

import flatland._

abstract class ConvexPolygonLike {
  def verticesCCW: Vec2Array // in counter clockwise order
  lazy val edges: IndexedSeq[Line] = Algorithms.polygonCornersToEdges(verticesCCW)

  // axis aligned bounding box
  def aabb = Algorithms.axisAlignedBoundingBox(verticesCCW)

  def intersect(line: Line) = Algorithms.intersect(this, line)

  def includes(v: Vec2): Boolean = edges.forall(_ leftOf v) // edges are ccw
  @inline def includes(l: Line): Boolean = includes(l.start) && includes(l.end)
  def intersectsMtd(that: ConvexPolygonLike): Option[Vec2] = Algorithms.intersect2ConvexPolygonMtd(this, that)
  def intersects(that: Circle): Boolean = Algorithms.intersectCircleConvexPolygon(this, that)
  def intersectsMtd(that: Circle): Option[Vec2] = Algorithms.intersectCircleConvexPolygonMtd(this, that, flip = false)
}

final case class ConvexPolygon(verticesCCW: Vec2Array) extends ConvexPolygonLike
