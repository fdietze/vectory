package vectory

import annotation.meta.field

import flatland._

final case class Circle(center: Vec2, r: Double) {
  @inline def x = center.x
  @inline def y = center.y
  @inline def d = r * 2

  @inline def circumferencePoint(angle: Double) = center + (Vec2.unit(angle) * r)

  def intersects(rect: AARect) = Algorithms.intersect(this, rect)
  def intersects(that: ConvexPolygonLike) = Algorithms.intersectCircleConvexPolygon(that, this)
  def intersectsMtd(that: ConvexPolygonLike): Option[Vec2] = Algorithms.intersectCircleConvexPolygonMtd(that, this, flip = true)
  def intersect(that: Line): Array[Vec2] = Algorithms.intersectCircleLine(this, that)
  def outerTangentCW(that: Circle) = Algorithms.circleOuterTangentCW(this, that)
  def outerTangentCCW(that: Circle) = Algorithms.circleOuterTangentCW(that, this).map(_.reversed)

  def includes(that: Circle): Boolean = {
    // https://stackoverflow.com/questions/33490334/check-if-a-circle-is-contained-in-another-circle/33490985#33490985
    val distanceSq = (this.center - that.center).lengthSq
    val differenceSq = (this.r - that.r) * (this.r - that.r)
    (differenceSq >= distanceSq) && (this.r >= that.r)
  }

  def sampleCircumference(n: Int): Vec2Array = {
    val samples = Vec2Array.create(n)
    val step = 2 * Math.PI / n
    var i = 0
    while (i < n) {
      val angle = step * i
      samples(i) = circumferencePoint(angle)
      i += 1
    }
    samples
  }
}
