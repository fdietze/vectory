package vectory

import annotation.meta.field

import flatland._

final case class Line(
  start: Vec2,
  end: Vec2
) {
  @inline def x1 = start.x
  @inline def y1 = start.y
  @inline def x2 = end.x
  @inline def y2 = end.y

  @inline def reversed = new Line(end, start)

  @inline def vector = end - start
  @inline def normal = vector.normal
  @inline def center = (start + end) * 0.5

  @inline def leftOf(p: Vec2) = (vector cross (p - start)) > 0
  @inline def rightOf(p: Vec2) = (vector cross (p - start)) <= 0

  @inline def apply(t: Double) = start + (vector * t)

  def distance(that: Vec2): Double = Algorithms.distancePointLine(that.x, that.y, x1, y1, x2, y2)
  def segmentDistance(that: Vec2): Double = Algorithms.distancePointLineSegment(that.x, that.y, x1, y1, x2, y2)
  def pointProjection(that: Vec2): Vec2 = Algorithms.projectPointOnLine(that.x, that.y, x1, y1, x2, y2)
  def intersect(that: Line): Option[Algorithms.LineIntersection] = Algorithms.intersect(this, that)
  def intersect(that: Circle): Array[Vec2] = Algorithms.intersectCircleLine(that, this)
  def intersect(r: ConvexPolygonLike): Either[Boolean, Seq[Vec2]] = Algorithms.intersect(r, this)
  def cutBy(r: ConvexPolygonLike): Option[Line] = Algorithms.cutLineByPolyAtStartOrEnd(this, r)
  def clampBy(r: ConvexPolygonLike): Option[Line] = Algorithms.clampLineByPoly(this, r)

  @inline def lengthSq = {
    val dx = start.x - end.x
    val dy = start.y - end.y
    dx * dx + dy * dy
  }

  @inline def length = Math.sqrt(lengthSq)
}

object Line {
  def apply(x1: Double, y1: Double, x2: Double, y2: Double) = new Line(Vec2(x1, y1), Vec2(x2, y2))
}

