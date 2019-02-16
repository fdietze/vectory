package vectory

import annotation.meta.field

import flatland._

//TODO: use Fast Inverse Square Root where possible
// for example Vec2.normalized
// https://en.wikipedia.org/wiki/Fast_inverse_square_root


object Algorithms {
  def polygonCornersToEdges(corners: IndexedSeq[Vec2]): IndexedSeq[Line] = {
    val n = corners.size
    val edges = new Array[Line](n)
    var i = 0
    var last = corners(n - 1)
    while (i < n) {
      val current = corners(i)
      edges(i) = Line(last, current)
      last = current
      i += 1
    }
    edges
  }

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

  def axisAlignedBoundingBox(vertices: IndexedSeq[Vec2]) = {
    val first = vertices(0)
    var xMin = first.x
    var xMax = xMin
    var yMin = first.y
    var yMax = yMin
    var i = 1
    val n = vertices.size
    while (i < n) {
      val vertex = vertices(i)
      val x = vertex.x
      val y = vertex.y
      if (x < xMin) xMin = x
      else if (x > xMax) xMax = x
      if (y < yMin) yMin = y
      else if (y > yMax) yMax = y
      i += 1
    }
    AARect(Vec2(xMin, yMin), Vec2(xMax, yMax))
  }

  final case class LineIntersection(pos: Vec2, onLine1: Boolean, onLine2: Boolean)
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

    if (denominator == 0) return None // lines are parallel

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

  def intersectCircleLine(circle: Circle, line: Line): Array[Vec2] = {
    // https://stackoverflow.com/questions/1073336/circle-line-segment-collision-detection-algorithm/1084899#1084899
    // The intersection points are ordered by the distance from line.start
    val d = line.vector
    val f = line.start - circle.center

    val a = d dot d
    val b = (f * 2) dot d
    val c = (f dot f) - circle.r * circle.r

    val discriminantSq = b * b - 4 * a * c
    if (discriminantSq < 0) Array.empty[Vec2]
    else {
      val discriminant = Math.sqrt(discriminantSq)
      if (discriminant == 0) {
        // tangent
        val t = (-b - discriminant) / (2 * a)
        Array(line(t))
      } else {
        // two intersection points
        val t1 = (-b - discriminant) / (2 * a)
        val t2 = (-b + discriminant) / (2 * a)
        Array(line.start + (d * t1), line.start + (d * t2))
      }
    }
  }

  def circleOuterTangentCW(c1: Circle, c2: Circle): Option[Line] = {
    if ((c1 includes c2) || (c2 includes c1)) return None

    // outer tangent with corrected atan sign: https://en.wikipedia.org/wiki/Tangent_lines_to_circles#Outer_tangent
    val x1 = c1.center.x
    val y1 = c1.center.y
    val r1 = c1.r
    val x2 = c2.center.x
    val y2 = c2.center.y
    val r2 = c2.r

    val gamma = -Math.atan2(y2 - y1, x2 - x1) // atan2 sets the correct sign, so that all tangents are on the right side of point order
    val beta = Math.asin((r2 - r1) / Math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)))
    val alpha = gamma - beta

    val x3 = x1 + r1 * Math.cos(Math.PI * 0.5 - alpha)
    val y3 = y1 + r1 * Math.sin(Math.PI * 0.5 - alpha)
    val x4 = x2 + r2 * Math.cos(Math.PI * 0.5 - alpha)
    val y4 = y2 + r2 * Math.sin(Math.PI * 0.5 - alpha)

    Some(Line(Vec2(x3, y3), Vec2(x4, y4)))
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
    intersectCircleAARect(circle.center.x, circle.center.y, circle.r, rect.center.x, rect.center.y, rect.size.width, rect.size.height)
  }

  def intersectCircleAARect(cx: Double, cy: Double, cr: Double, rcx: Double, rcy: Double, rw: Double, rh: Double): Boolean = {
    // https://stackoverflow.com/questions/401847/circle-rectangle-collision-detection-intersection/402010#402010
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

  def intersectCircleConvexPolygon(p: ConvexPolygonLike, c: Circle): Boolean = {
    if (p.includes(c.center)) return true
    p.edges.exists(segment => distancePointLineSegment(c.x, c.y, segment.x1, segment.y1, segment.x2, segment.y2) <= c.r)
  }

  def intersectCircleConvexPolygonMtd(p: ConvexPolygonLike, c: Circle, flip: Boolean): Option[Vec2] = {
    // https://github.com/snowkit/differ/blob/master/differ/sat/SAT2D.hx

    @inline def findNormalAxisX(verts: IndexedSeq[Vec2], index: Int): Double = {
      var v2 = if (index >= verts.length - 1) verts(0) else verts(index + 1)
      return -(v2.y - verts(index).y)
    }

    @inline def findNormalAxisY(verts: IndexedSeq[Vec2], index: Int): Double = {
      var v2 = if (index >= verts.length - 1) verts(0) else verts(index + 1)
      return (v2.x - verts(index).x)
    }

    val verts = p.verticesCCW
    val n = verts.size

    val circleX = c.x
    val circleY = c.y

    var testDistance = Double.MaxValue
    var distance = 0.0
    var closestX = 0.0
    var closestY = 0.0
    var overlap = 0.0
    var unitVectorX = 0.0
    var unitVectorY = 0.0
    var i = 0
    while (i < n) {
      distance = Vec2.lengthSq(circleX - verts(i).x, circleY - verts(i).y)

      if (distance < testDistance) {
        testDistance = distance
        closestX = verts(i).x
        closestY = verts(i).y
      }
      i += 1
    }

    var normalAxisX = closestX - circleX
    var normalAxisY = closestY - circleY
    var normAxisLen = Vec2.length(normalAxisX, normalAxisY)
    normalAxisX = Vec2.normalize(normAxisLen, normalAxisX)
    normalAxisY = Vec2.normalize(normAxisLen, normalAxisY)

    //project all its points, 0 outside the loop
    var test = 0.0
    var min1 = Vec2.dot(normalAxisX, normalAxisY, verts(0).x, verts(0).y)
    var max1 = min1

    var j = 0
    while (j < n) {
      test = Vec2.dot(normalAxisX, normalAxisY, verts(j).x, verts(j).y)
      if (test < min1) min1 = test
      if (test > max1) max1 = test
      j += 1
    }

    // project the circle
    var max2 = c.r
    var min2 = -c.r
    var offset = Vec2.dot(normalAxisX, normalAxisY, -circleX, -circleY)

    min1 += offset
    max1 += offset

    var test1 = min1 - max2
    var test2 = min2 - max1

    //if either test is greater than 0, there is a gap, we can give up now.
    if (test1 > 0 || test2 > 0) return None

    // circle distance check
    var distMin = -(max2 - min1)
    if (flip) distMin *= -1

    overlap = distMin
    unitVectorX = normalAxisX
    unitVectorY = normalAxisY
    var closest = Math.abs(distMin)

    // find the normal axis for each point and project
    i = 0
    while (i < n) {

      normalAxisX = findNormalAxisX(verts, i)
      normalAxisY = findNormalAxisY(verts, i)
      var aLen = Vec2.length(normalAxisX, normalAxisY)
      normalAxisX = Vec2.normalize(aLen, normalAxisX)
      normalAxisY = Vec2.normalize(aLen, normalAxisY)

      // project the polygon(again? yes, circles vs. polygon require more testing...)
      min1 = Vec2.dot(normalAxisX, normalAxisY, verts(0).x, verts(0).y)
      max1 = min1 //set max and min

      //project all the other points(see, cirlces v. polygons use lots of this...)
      j = 0
      while (j < n) {
        test = Vec2.dot(normalAxisX, normalAxisY, verts(j).x, verts(j).y)
        if (test < min1) min1 = test
        if (test > max1) max1 = test
        j += 1
      }

      // project the circle(again)
      max2 = c.r //max is radius
      min2 = -c.r //min is negative radius

      //offset points
      offset = Vec2.dot(normalAxisX, normalAxisY, -circleX, -circleY)
      min1 += offset
      max1 += offset

      // do the test, again
      test1 = min1 - max2
      test2 = min2 - max1

      //failed.. quit now
      if (test1 > 0 || test2 > 0) {
        return None
      }

      distMin = -(max2 - min1)
      if (flip) distMin *= -1

      if (Math.abs(distMin) < closest) {
        unitVectorX = normalAxisX
        unitVectorY = normalAxisY
        overlap = distMin
        closest = Math.abs(distMin)
      }

      i += 1
    }

    //if you made it here, there is a collision!!!!!

    // shape1 = if (flip) polygon else c
    // shape2 = if (flip) c else polygon
    val separationX = unitVectorX * overlap
    val separationY = unitVectorY * overlap

    return Some(Vec2(separationX, separationY))
  }

  // returns shortest vector to separate polygons
  def intersect2ConvexPolygonMtd(a: ConvexPolygonLike, b: ConvexPolygonLike): Option[Vec2] = {
    // https://stackoverflow.com/questions/753140/how-do-i-determine-if-two-convex-polygons-intersect
    // http://gamemath.com/2011/09/detecting-whether-two-convex-polygons-overlap
    // http://elancev.name/oliver/2D%20polygon.htm#tut2
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

    var shortestAxis: Vec2 = null
    var shortestAxisLengthSq: Double = Double.MaxValue

    def separatingAxis(edge: Line, a: ConvexPolygonLike, b: ConvexPolygonLike): Boolean = {
      val axis = edge.normal
      val (aMin, aMax) = projectionExtents(axis, a.verticesCCW)
      val (bMin, bMax) = projectionExtents(axis, b.verticesCCW)
      if (aMax < bMin || bMax < aMin) return true

      val d0 = aMax - bMin
      val d1 = bMax - aMin
      var flip = 1
      val depth = if (d0 < d1) d0 else { flip = -1; d1 }

      val pushVector = axis * (depth * flip / axis.lengthSq)
      val pushVectorLengthSq = pushVector.lengthSq
      if (pushVectorLengthSq < shortestAxisLengthSq) {
        shortestAxis = pushVector
        shortestAxisLengthSq = pushVectorLengthSq
      }

      return false
    }

    val noSeparatingAxisFound = {
      a.edges.forall (!separatingAxis(_, a, b)) &&
        b.edges.forall (!separatingAxis(_, a, b))
    }

    if (noSeparatingAxisFound) {
      // overlapping, provide vector to separate
      Some(shortestAxis)
    } else {
      // not overlapping
      None
    }
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

