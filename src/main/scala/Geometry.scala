package vectory

import scala.scalajs.js
import scala.scalajs.js.annotation.{ JSExport, JSExportAll }
import annotation.meta.field

case class Vec2(x: Double, y: Double) {
  def width = x
  def height = y

  def unary_- = Vec2(-x, -y)
  def abs = Vec2(Math.abs(x), Math.abs(y))

  def +(that: Vec2) = Vec2(this.x + that.x, this.y + that.y)
  def +(that: Double) = Vec2(this.x + that, this.y + that)
  def -(that: Vec2) = Vec2(this.x - that.x, this.y - that.y)
  def -(that: Double) = Vec2(this.x - that, this.y - that)
  def *(a: Double) = Vec2(this.x * a, this.y * a)
  def /(a: Double) = Vec2(this.x / a, this.y / a)
  def dot(that: Vec2) = this.x * that.x + this.y * that.y
  def cross(that: Vec2) = this.x * that.y - this.y * that.x

  def lengthSq = x * x + y * y
  def length = Math.sqrt(lengthSq)
  def normalized = this / length
  def area = x * y
  def normal = Vec2(y, -x)

  def angle = Math.atan2(y, x)

  def toTuple = (x, y)
}

object Vec2 {
  def apply(tuple: (Double, Double)) = new Vec2(tuple._1, tuple._2)
  def apply(x: Double) = new Vec2(x, x)
  def apply(v: { def x: Double; def y: Double }) = new Vec2(v.x, v.y)
  def dim(v: { def width: Double; def height: Double }) = new Vec2(v.width, v.height)
}

case class Line(
  start: Vec2,
  end:   Vec2
) {
  def x1 = start.x
  def y1 = start.y
  def x2 = end.x
  def y2 = end.y

  def vector = end - start
  def normal = vector.normal
  def center = (start + end) / 2

  def leftOf(p: Vec2) = (vector cross (p - start)) > 0
  def rightOf(p: Vec2) = !leftOf(p)

  def distance(that: Vec2): Double = Algorithms.distancePointLine(that.x, that.y, x1, y1, x2, y2)
  def segmentDistance(that: Vec2): Double = Algorithms.distancePointLineSegment(that.x, that.y, x1, y1, x2, y2)
  def pointProjection(that: Vec2): Vec2 = Algorithms.projectPointOnLine(that.x, that.y, x1, y1, x2, y2)
  def intersect(that: Line): Option[Algorithms.LineIntersection] = Algorithms.intersect(this, that)
  def intersect(r: ConvexPolygonLike): Either[Boolean, Seq[Vec2]] = Algorithms.intersect(r, this)
  def cutBy(r: ConvexPolygonLike): Option[Line] = Algorithms.cutLineByPolyAtStartOrEnd(this, r)
  def clampBy(r: ConvexPolygonLike): Option[Line] = Algorithms.clampLineByPoly(this, r)

  def lengthSq = {
    val dx = start.x - end.x
    val dy = start.y - end.y
    dx * dx + dy * dy
  }

  def length = Math.sqrt(lengthSq)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Line]

  override def equals(other: Any): Boolean = other match {
    case that: Line => (that canEqual this) &&
      (this.start == that.start && this.end == that.end) ||
      (this.start == that.end && this.end == that.start)
    case _ => false
  }

  override def hashCode = start.hashCode * end.hashCode // multiply to be commutative
}

case class Circle(center: Vec2, r: Double) {
  def x = center.x
  def y = center.y
  def d = r * 2

  def intersects(rect: AARect) = Algorithms.intersect(this, rect)
  def intersects(that: ConvexPolygonLike) = Algorithms.intersectCircleConvexPolygon(this, that)
}

trait ConvexPolygonLike {
  def cornersCCW: IndexedSeq[Vec2] // in counter clockwise order
  lazy val edges: Seq[Line] = Algorithms.slidingRotate(cornersCCW).map(e => Line(e.head, e.last))

  // axis aligned bounding box
  def aabb = {
    val first = cornersCCW(0)
    var xMin = first.x
    var xMax = xMin
    var yMin = first.y
    var yMax = yMin
    var i = 1
    val n = cornersCCW.size
    while (i < n) {
      val vertex = cornersCCW(i)
      val x = vertex.x
      val y = vertex.y
      if (x < xMin) xMin = x
      else if (x > xMax) xMax = x
      if (y < yMin) yMin = y
      else if (y > yMax) yMax = y
      i += 1
    }
    AARect(Vec2(xMin, yMin), Vec2(xMax - xMin, yMax - yMin))
  }

  def intersect(line: Line) = Algorithms.intersect(this, line)

  def includes(v: Vec2): Boolean = edges.forall(_ rightOf v)
  def includes(l: Line): Boolean = includes(l.start) && includes(l.end)
  def intersects(that: ConvexPolygonLike): Boolean = Algorithms.intersect2ConvexPolygon(this, that)
  def intersects(that: Circle) = Algorithms.intersectCircleConvexPolygon(that, this)
}

case class ConvexPolygon(cornersCCW: IndexedSeq[Vec2]) extends ConvexPolygonLike

trait Rect extends ConvexPolygonLike {
  def center: Vec2
  def x = center.x
  def y = center.y

  def size: Vec2
  def width = size.x
  def height = size.y

  def angle: Double

  def minCorner: Vec2
  def maxCorner: Vec2
}

object Rect {
  def apply(center: Vec2, size: Vec2, angle: Double = 0): Rect = if (angle == 0) AARect(center, size) else RotatedRect(center, size, angle)
}

case class RotatedRect(center: Vec2, size: Vec2, angle: Double) extends Rect {
  import Math.{ sin, cos }

  lazy val toRight = Vec2(cos(angle), sin(angle)) * (width / 2)
  lazy val toBottom = Vec2(-sin(angle), cos(angle)) * (height / 2)

  lazy val minCorner = center - toRight - toBottom
  lazy val maxCorner = center + toRight + toBottom

  lazy val cornersCCW = Vector(
    minCorner,
    center - toRight + toBottom,
    maxCorner,
    center + toRight - toBottom
  )
}

case class AARect(center: Vec2, size: Vec2) extends Rect {
  override def angle = 0

  lazy val minCorner = center - size / 2
  lazy val maxCorner = center + size / 2

  override def aabb = this

  override def includes(v: Vec2): Boolean = v.x > minCorner.x && v.y > minCorner.y && v.x < maxCorner.x && v.y < maxCorner.y

  lazy val cornersCCW = Vector(
    minCorner,
    minCorner + Vec2(size.x, 0),
    maxCorner,
    minCorner + Vec2(0, size.y)
  )

  override def intersects(that: ConvexPolygonLike): Boolean = that match {
    case that: AARect =>
      ((this.x < that.x + that.width) && (this.x + this.width > that.x)) &&
        ((this.y < that.y + that.width) && (this.y + this.width > that.y))
    case poly => Algorithms.intersect2ConvexPolygon(this, that)
  }

  override def intersects(circle: Circle) = Algorithms.intersect(circle, this)
}

object Algorithms {
  def slidingRotate[T](l: Seq[T]): IndexedSeq[Seq[T]] = (l :+ l.head).sliding(2).toIndexedSeq

  def distancePointLine(x0: Double, y0: Double, x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    // Point: x0, y0
    // Line: x1, y1 --- x2, y2
    Math.abs((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1) / Math.sqrt((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1))
  }

  def distancePointLineSegment(x0: Double, y0: Double, x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    import Math.{ min, max }
    // https://stackoverflow.com/questions/849211/shortest-distance-between-a-point-and-a-line-segment
    // Return minimum distance between line segment vw and point p
    val p = Vec2(x0, y0)
    val v = Vec2(x1, y1)
    val w = Vec2(x2, y2)
    val l2 = Line(v, w).lengthSq // i.e. |w-v|^2 -  avoid a sqrt
    if (l2 == 0.0) return Line(p, v).length // v == w case
    // Consider the line extending the segment, parameterized as v + t (w - v).
    // We find projection of point p onto the line.
    // It falls where t = [(p-v) . (w-v)] / |w-v|^2
    // We clamp t from [0,1] to handle points outside the segment vw.
    val t = max(0, min(1, ((p - v) dot (w - v)) / l2))
    val projection = v + (w - v) * t // Projection falls on the segment
    return Line(p, projection).length
  }

  def projectPointOnLine(x0: Double, y0: Double, x1: Double, y1: Double, x2: Double, y2: Double): Vec2 = {
    val m = (y2 - y1) / (x2 - x1)
    val b = y1 - m * x1
    val x = (m * y0 + x0 - m * b) / (m * m + 1)
    val y = (m * m * y0 + m * x0 + b) / (m * m + 1)
    Vec2(x, y)
  }

  case class LineIntersection(pos: Vec2, onLine1: Boolean, onLine2: Boolean)
  def intersect(line1: Line, line2: Line): Option[LineIntersection] = {
    // if the lines intersect, the result contains the x and y of the intersection
    // (treating the lines as infinite) and booleans for
    // whether line segment 1 or line segment 2 contain the point
    // from: http://jsfiddle.net/justin_c_rounds/Gd2S2
    // ported to scala

    val line1Dx = line1.end.x - line1.start.x
    val line1Dy = line1.end.y - line1.start.y
    val line2Dx = line2.end.x - line2.start.x
    val line2Dy = line2.end.y - line2.start.y

    val denominator = (line2Dy * line1Dx) - (line2Dx * line1Dy)

    if (denominator == 0) return None

    val startDx = line1.start.x - line2.start.x
    val startDy = line1.start.y - line2.start.y

    val numerator1 = (line2Dx * startDy) - (line2Dy * startDx)
    val numerator2 = (line1Dx * startDy) - (line1Dy * startDx)
    val a = numerator1 / denominator
    val b = numerator2 / denominator

    // if we cast these lines infinitely in both directions, they intersect here:
    val resultX = line1.start.x + (a * (line1Dx))
    val resultY = line1.start.y + (a * (line1Dy))
    /*
    // it is worth noting that this should be the same as:
    x = line2StartX + (b * (line2EndX - line2StartX))
    y = line2StartX + (b * (line2EndY - line2StartY))
    */
    // if line1 is a segment and line2 is infinite, they intersect if:
    val resultOnLine1 = a > 0 && a < 1
    // if line2 is a segment and line1 is infinite, they intersect if:
    val resultOnLine2 = b > 0 && b < 1
    // if line1 and line2 are segments, they intersect if both of the above are true
    return Some(LineIntersection(Vec2(resultX, resultY), resultOnLine1, resultOnLine2))
  }

  def intersect(poly: ConvexPolygonLike, line: Line): Either[Boolean, Seq[Vec2]] = {
    // Left(true)  => line is completely inside
    // Left(false) => line is completely outside
    // Right(pos)  => one intersection point
    val intersections = poly.edges.flatMap { edge =>
      (line intersect edge).filter(i => i.onLine1 && i.onLine2).map(_.pos)
    }

    if (intersections.nonEmpty)
      Right(intersections)
    else Left(poly includes line)
  }

  def intersect(circle: Circle, rect: AARect): Boolean = {
    // https://stackoverflow.com/questions/401847/circle-rectangle-collision-detection-intersection/402010#402010
    // val circleDistance = (circle.center - rect.center).abs

    //  if (circleDistance.x > (rect.width/2 + circle.r)) return false
    //  if (circleDistance.y > (rect.height/2 + circle.r)) return false

    //  if (circleDistance.x <= (rect.width/2)) return true
    //  if (circleDistance.y <= (rect.height/2)) return true

    //  val cornerDistance_sq = (circleDistance - rect.size / 2).lengthSq

    //  return cornerDistance_sq <= circle.r*circle.r
    intersectCircleAARect(circle.center.x, circle.center.y, circle.r, rect.center.x, rect.center.y, rect.size.width, rect.size.height)
  }

  def intersectCircleAARect(cx: Double, cy: Double, cr: Double, rcx: Double, rcy: Double, rw: Double, rh: Double): Boolean = {

    val circleDistanceX = Math.abs(cx - rcx)
    val circleDistanceY = Math.abs(cy - rcy)

    val rwh = rw * 0.5
    val rhh = rh * 0.5

    if (circleDistanceX > (rwh + cr)) return false
    if (circleDistanceY > (rhh + cr)) return false

    if (circleDistanceX <= (rwh)) return true
    if (circleDistanceY <= (rhh)) return true

    val cornerDistanceX = circleDistanceX - rwh
    val cornerDistanceY = circleDistanceY - rhh
    val cornerDistance_sq = cornerDistanceX * cornerDistanceX + cornerDistanceY * cornerDistanceY

    return cornerDistance_sq <= cr * cr
  }

  def intersectCircleConvexPolygon(c: Circle, p: ConvexPolygonLike): Boolean = {
    if (p.includes(c.center)) return true
    p.edges.exists(segment => distancePointLineSegment(c.x, c.y, segment.x1, segment.y1, segment.x2, segment.y2) <= c.r)
  }

  def intersect2ConvexPolygon(a: ConvexPolygonLike, b: ConvexPolygonLike): Boolean = {
    // https://stackoverflow.com/questions/753140/how-do-i-determine-if-two-convex-polygons-intersect
    // http://gamemath.com/2011/09/detecting-whether-two-convex-polygons-overlap
    def projectionExtents(axis: Vec2, vertices: IndexedSeq[Vec2]): (Double, Double) = {
      var aMin = axis dot vertices(0)
      var aMax = aMin
      var i = 1
      val n = vertices.size
      while (i < n) {
        val d = axis dot vertices(i)
        if (d < aMin) aMin = d
        else if (d > aMax) aMax = d
        i += 1
      }
      (aMin, aMax)
    }

    def separatingAxis(edge: Line, a: ConvexPolygonLike, b: ConvexPolygonLike) = {
      val axis = edge.normal
      val (aMin, aMax) = projectionExtents(axis, a.cornersCCW)
      val (bMin, bMax) = projectionExtents(axis, b.cornersCCW)
      aMax < bMin || bMax < aMin
    }

    a.edges.forall (!separatingAxis(_, a, b)) &&
      b.edges.forall (!separatingAxis(_, a, b))
  }

  def cutLineByPolyAtStartOrEnd(line: Line, poly: ConvexPolygonLike): Option[Line] = {
    // Assuming there is only one intersection.
    // Which means that one line end is inside the poly,
    // the other one outside.
    // If there are two intersections the resulting line
    // can be wrong
    intersect(poly, line) match {
      case Left(true)  => None // line inside
      case Left(false) => Some(line) // line outside
      case Right(intersections) =>
        // with the assumption that the poly covers one line end,
        // we have exactly one intersection
        if (poly includes line.end)
          Some(Line(line.start, intersections.head))
        else
          Some(Line(intersections.head, line.end))
    }
  }

  def clampLineByPoly(line: Line, poly: ConvexPolygonLike): Option[Line] = {
    (poly includes line.start, poly includes line.end) match {
      case (true, true)  => Some(line)
      case (true, false) => Some(Line(line.start, intersect(poly, line).right.get.head))
      case (false, true) => Some(Line(intersect(poly, line).right.get.head, line.end))
      case (false, false) =>
        intersect(poly, line) match {
          case Left(_) => None
          case Right(intersections) =>
            // polygon is convex, line endpoints lie outside,
            // so we have exactly two intersections
            Some(Line(intersections(0), intersections(1)))
        }
    }

  }

  def convexHull(points: Iterable[Vec2]) = ConvexHull2D(points)
}
