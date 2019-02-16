package vectory

import org.scalatest._

class GeometrySpec extends FreeSpec with MustMatchers {
  "Primitives" - {
    "Vec2" - {
      "constructor" in {
        val v = Vec2(5, 7)
        v.x mustEqual 5
        v.y mustEqual 7
        v.width mustEqual 5
        v.height mustEqual 7
      }
      "factory from one argument" in {
        val v = Vec2(5)
        v.x mustEqual 5
        v.y mustEqual 5
      }
      "factory from structural type: x, y" in {
        val v = Vec2(new { val x = 5.0; val y = 7.0 })
        v.x mustEqual 5
        v.y mustEqual 7
      }
      "factory from structural type: width, height" in {
        val v = Vec2.dim(new { val width = 5.0; val height = 7.0 })
        v.x mustEqual 5
        v.y mustEqual 7
      }
      "from tuple" in {
        val v = Vec2((5.0, 7.0))
        v.x mustEqual 5
        v.y mustEqual 7
      }
      "to tuple" in {
        val v = Vec2(5, 7)
        v.toTuple mustEqual ((5, 7))
      }
      "addition" in {
        val a = Vec2(5, 7)
        val b = Vec2(2, 3)
        val c = a + b
        c.x mustEqual 7
        c.y mustEqual 10
      }
      "scalar addition" in {
        val a = Vec2(5, 7)
        val b = 3
        val c = a + b
        c.x mustEqual 8
        c.y mustEqual 10
      }
      "substraction" in {
        val a = Vec2(5, 7)
        val b = Vec2(2, 3)
        val c = a - b
        c.x mustEqual 3
        c.y mustEqual 4
      }
      "scalar substraction" in {
        val a = Vec2(5, 7)
        val b = 3
        val c = a - b
        c.x mustEqual 2
        c.y mustEqual 4
      }
      "multiplication" in {
        val a = Vec2(5, 7)
        val c = a * 3
        c.x mustEqual 15
        c.y mustEqual 21
      }
      "division" in {
        val a = Vec2(6, 8)
        val c = a / 2
        c.x mustEqual 3
        c.y mustEqual 4
      }
      "unary -" in {
        val a = -Vec2(6, 8)
        a.x mustEqual -6
        a.y mustEqual -8
      }
      "abs" in {
        val a = Vec2(6, 8).abs
        a.x mustEqual 6
        a.y mustEqual 8
        val b = Vec2(-6, -8).abs
        b.x mustEqual 6
        b.y mustEqual 8
      }
      "crossProduct" in {
        val a = Vec2(6, 8)
        val b = Vec2(2, 3)
        (a cross b) mustEqual 2
      }
      "dotProduct" in {
        val a = Vec2(6, 8)
        val b = Vec2(2, 3)
        (a dot b) mustEqual 36
      }
      "length" in {
        Vec2(3, 4).lengthSq mustEqual 25
        Vec2(3, 4).length mustEqual 5
      }
      "normalized" in {
        Vec2(3, 4).normalized.length mustEqual 1
        Vec2(-2, 3).normalized.length mustEqual 1
      }
      "area" in {
        Vec2(3, 4).area mustEqual 12
        Vec2(3, 0).area mustEqual 0
      }
    }

    "Line" - {
      "constructor" in {
        val l = Line(Vec2(7, 3), Vec2(8, 4))
        l.start mustEqual Vec2(7, 3)
        l.end mustEqual Vec2(8, 4)
        l.x1 mustEqual 7
        l.y1 mustEqual 3
        l.x2 mustEqual 8
        l.y2 mustEqual 4
      }

      "vector" in {
        val l = Line(Vec2(7, 3), Vec2(8, 5))
        l.vector mustEqual Vec2(1, 2)
      }

      "center" in {
        val l = Line(Vec2(7, 3), Vec2(8, 5))
        l.center mustEqual Vec2(7.5, 4)
      }

      "angle" in {
        val l = Line(Vec2(7, 3), Vec2(12, 3))
        l.vector.angle mustEqual 0
        val m = Line(Vec2(7, 3), Vec2(6, 3))
        m.vector.angle mustEqual Math.PI
        val n = Line(Vec2(7, 3), Vec2(7, 5))
        n.vector.angle mustEqual Math.PI / 2
        val o = Line(Vec2(7, 3), Vec2(7, 1))
        o.vector.angle mustEqual -Math.PI / 2
      }

      "rightOf" in {
        val l = Line(Vec2(4, 12), Vec2(8, 0))
        val p = Vec2(12, 16)
        (l rightOf p) mustEqual false
        (l leftOf p) mustEqual true
      }

      "length" in {
        val l = Line(Vec2(5, 5), Vec2(8, 9))
        l.length mustEqual 5
      }

      "distance to point" - {
        "distance to point (point on line)" in {
          val l = Line(Vec2(5, 5), Vec2(9, 5))
          val p = Vec2(7, 5)
          l.distance(p) mustEqual 0
        }

        "distance to point 1" in {
          val l = Line(Vec2(5, 5), Vec2(9, 5))
          val p = Vec2(7, 3)
          l.distance(p) mustEqual 2
        }

        "distance to point 2" in {
          val l = Line(Vec2(5, 5), Vec2(9, 5))
          val p = Vec2(7, 8)
          l.distance(p) mustEqual 3
        }
        "distance to point 3" in {
          val l = Line(Vec2(-1, -1), Vec2(1, 3))
          val p = Vec2(4, -1)
          l.distance(p) mustEqual Math.sqrt(20)
        }
      }

      "segment distance to point" - {
        "point on segment" in {
          val l = Line(Vec2(5, 5), Vec2(9, 5))
          val p = Vec2(7, 5)
          l.segmentDistance(p) mustEqual 0
        }

        "point on line" in {
          val l = Line(Vec2(5, 5), Vec2(9, 5))
          val p = Vec2(3, 5)
          l.segmentDistance(p) mustEqual 2
        }

        "segment start to point" in {
          val l = Line(Vec2(5, 5), Vec2(9, 5))
          val p = Vec2(3, 3)
          l.segmentDistance(p) mustEqual Math.sqrt(8)
        }

        "segment end to point" in {
          val l = Line(Vec2(5, 5), Vec2(9, 5))
          val p = Vec2(11, 3)
          l.segmentDistance(p) mustEqual Math.sqrt(8)
        }

        "segment distance to point 1" in {
          val l = Line(Vec2(5, 5), Vec2(9, 5))
          val p = Vec2(7, 3)
          l.segmentDistance(p) mustEqual 2
        }

        "segment distance to point 2" in {
          val l = Line(Vec2(5, 5), Vec2(9, 5))
          val p = Vec2(7, 8)
          l.segmentDistance(p) mustEqual 3
        }
        "segment distance to point 3" in {
          val l = Line(Vec2(-1, -1), Vec2(1, 3))
          val p = Vec2(4, -1)
          l.segmentDistance(p) mustEqual Math.sqrt(20)
        }
      }

      "project point on line (already on line)" in {
        val l = Line(Vec2(5, 5), Vec2(9, 5))
        val p = Vec2(7, 5)
        l.pointProjection(p) mustEqual Vec2(7, 5)
      }

      "project point on line 1" in {
        val l = Line(Vec2(5, 5), Vec2(9, 5))
        val p = Vec2(7, 3)
        l.pointProjection(p) mustEqual Vec2(7, 5)
      }

      "project point on line 2" in {
        val l = Line(Vec2(5, 5), Vec2(9, 5))
        val p = Vec2(7, 8)
        l.pointProjection(p) mustEqual Vec2(7, 5)
      }
      "project point on line 3" in {
        val l = Line(Vec2(-1, -1), Vec2(1, 3))
        val p = Vec2(4, -1)
        l.pointProjection(p) mustEqual Vec2(0, 1)
      }
    }

    "Circle" - {
      "constructor" in {
        val c = Circle(Vec2(2, 3), 5)
        c.center mustEqual Vec2(2, 3)
        c.x mustEqual 2
        c.y mustEqual 3
        c.r mustEqual 5
      }
      "diameter" in {
        val c = Circle(Vec2(2, 3), 5)
        c.d mustEqual 10
      }

      "sampleCircumference" in {
        val c = Circle(Vec2(2, 3), 5)
        c.sampleCircumference(4).toList mustEqual List(Vec2(7.0, 3.0), Vec2(2.0000000000000004, 8.0), Vec2(-3.0, 3.0000000000000004), Vec2(1.9999999999999991, -2.0))
      }

      "intersects with polygon" - {
        "no intersection" in {
          val p = ConvexPolygon(Vec2Array(Vec2(-3, 1), Vec2(-1, -2), Vec2(2, 2)))
          val c = Circle(Vec2(2, -2), 1)
          (p intersects c) mustEqual false
          (c intersects p) mustEqual false
          (p intersectsMtd c) mustEqual None
          (c intersectsMtd p) mustEqual None
        }

        "circle touches polygon at line" in {
          val p = ConvexPolygon(Vec2Array(Vec2(-3, 1), Vec2(-1, -2), Vec2(2, 2)))
          val c = Circle(Vec2(1, -1), 2)
          (p intersects c) mustEqual true
          (c intersects p) mustEqual true
          (p intersectsMtd c) mustEqual Some(Vec2(0.8000000000000003, -0.6000000000000001))
          (c intersectsMtd p) mustEqual Some(Vec2(-0.8000000000000003, 0.6000000000000001))
        }

        "circle touches polygon at corner" in {
          val p = ConvexPolygon(Vec2Array(Vec2(-3, 1), Vec2(-1, -2), Vec2(2, 2)))
          val c = Circle(Vec2(-1, -4), 3)
          (p intersects c) mustEqual true
          (c intersects p) mustEqual true
          (p intersectsMtd c) mustEqual Some(Vec2(0, -1))
          (c intersectsMtd p) mustEqual Some(Vec2(0, 1))
        }

        "circle not touching at corner" in {
          val p = ConvexPolygon(Vec2Array(Vec2(-3, 1), Vec2(-1, -2), Vec2(2, 2)))
          val c = Circle(Vec2(-1, -5), 1)
          (p intersects c) mustEqual false
          (c intersects p) mustEqual false
          (p intersectsMtd c) mustEqual None
          (c intersectsMtd p) mustEqual None
        }

        "circle completely inside polygon" in {
          val p = ConvexPolygon(Vec2Array(Vec2(-3, 1), Vec2(-1, -4), Vec2(3, -5), Vec2(5, -1), Vec2(2, 2)))
          val c = Circle(Vec2(2, -2), 2)
          (p intersects c) mustEqual true
          (c intersects p) mustEqual true
          (p intersectsMtd c) mustEqual Some(Vec2(3.788854381999832, -1.894427190999916))
          (c intersectsMtd p) mustEqual Some(Vec2(-3.788854381999832, 1.894427190999916))
        }

        "circle completely inside large polygon" in {
          val p = ConvexPolygon(Vec2Array(Vec2(268.3136177218223, 146.00249872072908), Vec2(266.93449214067226, 139.0691662220126), Vec2(263.0070747651478, 133.19137074541013), Vec2(257.1292792885453, 129.26395336988566), Vec2(-255.62455212282495, -206.02867772286498), Vec2(-271.6207653711504, -209.21052236919925), Vec2(-287.6169786194759, -206.02867772286498), Vec2(-301.1779134067779, -196.96755078777306), Vec2(-310.2390403418699, -183.40661600047096), Vec2(-313.42088498820414, -167.41040275214553), Vec2(-320.774494668978, 139.92789248579257), Vec2(-319.3953692000187, 146.86122442048787), Vec2(-315.46795214398657, 152.73901941893632), Vec2(-309.5901571455381, 156.66643647496846), Vec2(-302.65682521084284, 158.04556194392768), Vec2(250.19594678982887, 164.12016965272252), Vec2(257.1292792885453, 162.7410440715725), Vec2(263.0070747651478, 158.81362669604803), Vec2(266.9344921406723, 152.93583121944553)))
          val c = Circle(Vec2(-58.538346206308354, 134.5704658111136), 168.11767240585183)
          (p intersects c) mustEqual true
          (c intersects p) mustEqual true
        }
      }
    }

    "ConvexPolygon" - {
      "point inside" in {
        val p = ConvexPolygon(Vec2Array(Vec2(-3, 1), Vec2(-1, -4), Vec2(3, -5), Vec2(5, -1), Vec2(2, 2)))
        val c = Circle(Vec2(2, -2), 2)
        p.includes(c.center) mustEqual true
      }

      "intersect 2 convex polygons" - {
        "no intersection" in {
          val a = ConvexPolygon(Vec2Array(Vec2(-3, 1), Vec2(-1, -2), Vec2(2, 2)))
          val b = ConvexPolygon(Vec2Array(Vec2(0, -1), Vec2(2, -1), Vec2(2, 1)))
          (a intersectsMtd b) mustEqual None
          (b intersectsMtd a) mustEqual None
        }
        "completely inside" in {
          val a = ConvexPolygon(Vec2Array(Vec2(-3, 1), Vec2(-1, -4), Vec2(3, -5), Vec2(5, -1), Vec2(2, 2)))
          val b = ConvexPolygon(Vec2Array(Vec2(0, -1), Vec2(2, -1), Vec2(2, 1)))
          (a intersectsMtd b) mustEqual Some(Vec2(-0.5769230769230769, 2.884615384615384))
          (b intersectsMtd a) mustEqual Some(Vec2(0.5769230769230769, -2.884615384615384))
        }
        "overlapping" in {
          val a = ConvexPolygon(Vec2Array(Vec2(-3, 1), Vec2(-1, -2), Vec2(2, 2)))
          val b = ConvexPolygon(Vec2Array(Vec2(0, 0), Vec2(2, -1), Vec2(2, 1)))
          (a intersectsMtd b) mustEqual Some(Vec2(0.32, -0.24))
          (b intersectsMtd a) mustEqual Some(Vec2(-0.32, 0.24))
        }
      }

      "axis aligned bounding box" in {
        val p = ConvexPolygon(Vec2Array(Vec2(-2, -2), Vec2(-3, -1), Vec2(2, -2), Vec2(1, 3)))
        p.aabb mustEqual AARect.fromCenter(Vec2(-0.5, 0.5), Vec2(5, 5))
      }
    }

    "AARect" - {
      "constructor" in {
        val r = AARect.fromCenter(Vec2(11, 5), Vec2(8, 4))
        r.center mustEqual Vec2(11, 5)
        r.size mustEqual Vec2(8, 4)
        r.width mustEqual 8
        r.height mustEqual 4
      }

      "minMaxCorner" in {
        val r = AARect.fromCenter(Vec2(11, 5), Vec2(8, 4))
        r.minCorner mustEqual Vec2(7, 3)
        r.maxCorner mustEqual Vec2(15, 7)
      }

      "verticesCCW" in {
        val r = AARect.fromCenter(Vec2(3, 3.5), Vec2(2, 1))
        val c = r.verticesCCW
        c.toList mustEqual List(Vec2(2, 3), Vec2(4, 3), Vec2(4, 4), Vec2(2, 4))
      }

      "edges" in {
        val r = AARect.fromCenter(Vec2(3, 3.5), Vec2(2, 1))
        val e = r.edges.toList
        e mustEqual List(
          Line(Vec2(2, 4), Vec2(2, 3)),
          Line(Vec2(2, 3), Vec2(4, 3)),
          Line(Vec2(4, 3), Vec2(4, 4)),
          Line(Vec2(4, 4), Vec2(2, 4))
        )
      }

      "PointInside" in {
        val r = AARect.fromCenter(Vec2(3, 3.5), Vec2(2, 1))
        (r includes Vec2(3, 3.5)) mustEqual true
        (r includes Vec2(3, 4.5)) mustEqual false
      }

      "LineInside" in {
        val r = AARect.fromCenter(Vec2(3, 3.5), Vec2(2, 1))
        (r includes Line(Vec2(2.5, 3.5), Vec2(3.5, 3.5))) mustEqual true
        (r includes Line(Vec2(2.5, 3.5), Vec2(5.5, 3.5))) mustEqual false
        (r includes Line(Vec2(4.5, 3.5), Vec2(5.5, 3.5))) mustEqual false
      }
      "OverlappingRect" in {
        val r1 = AARect.fromCenter(Vec2(2, 3), Vec2(4, 4))
        val r2 = AARect.fromCenter(Vec2(1, 4), Vec2(3, 1))
        val r3 = AARect.fromCenter(Vec2(10, 10), Vec2(1, 1))
        (r1 intersectsMtd r2) mustEqual Some(Vec2(0, 1.5))
        (r2 intersectsMtd r1) mustEqual Some(Vec2(0, -1.5))
        (r1 intersectsMtd r3) mustEqual None
        (r3 intersectsMtd r1) mustEqual None
      }
    }

    "RotatedRect" - {
      "constructor" in {
        val r = RotatedRect(Vec2(11, 5), Vec2(8, 4), Math.PI / 4)
        r.center mustEqual Vec2(11, 5)
        r.size mustEqual Vec2(8, 4)
        r.width mustEqual 8
        r.height mustEqual 4
      }

      "minMaxCorner 1" in {
        val r = RotatedRect(Vec2(0, 0), Vec2(2, 2), 0)
        r.minCorner mustEqual Vec2(-1, -1)
        r.maxCorner mustEqual Vec2(1, 1)
      }

      "minMaxCorner 2" in {
        val r = RotatedRect(Vec2(8, 9.5), Vec2(20, 5), Math.atan(4.0 / 3.0))
        val roundedMin = Vec2(Math.round(r.minCorner.x), Math.round(r.minCorner.y))
        val roundedMax = Vec2(Math.round(r.maxCorner.x), Math.round(r.maxCorner.y))
        roundedMin mustEqual Vec2(4, 0)
        roundedMax mustEqual Vec2(12, 19)
      }

      "verticesCCW" in {
        val r = RotatedRect(Vec2(8, 9.5), Vec2(20, 5), Math.atan(4.0 / 3.0))
        val rounded = r.verticesCCW.map(v => Vec2(Math.round(v.x), Math.round(v.y)))
        rounded.toList mustEqual List(Vec2(4, 0), Vec2(16, 16), Vec2(12, 19), Vec2(0, 3))
      }

      "edges" in {
        val r = RotatedRect(Vec2(8, 9.5), Vec2(20, 5), Math.atan(4.0 / 3.0))
        val e = r.edges.toList
        val rounded = e.toList.map(l => Line(
          Vec2(Math.round(l.start.x), Math.round(l.start.y)),
          Vec2(Math.round(l.end.x), Math.round(l.end.y))
        ))
        rounded mustEqual List(
          Line(Vec2(0, 3), Vec2(4, 0)),
          Line(Vec2(4, 0), Vec2(16, 16)),
          Line(Vec2(16, 16), Vec2(12, 19)),
          Line(Vec2(12, 19), Vec2(0, 3))
        )
      }
      "PointInside" in {
        val r = RotatedRect(Vec2(8, 9.5), Vec2(20, 5), Math.atan(4.0 / 3.0))
        (r includes Vec2(4, 4)) mustEqual true
        (r includes Vec2(8, 12)) mustEqual true
        (r includes Vec2(12, 8)) mustEqual false
        (r includes Vec2(12, 10)) mustEqual false
      }

      "LineInside" in {
        val r = RotatedRect(Vec2(8, 9.5), Vec2(20, 5), Math.atan(4.0 / 3.0))
        (r includes Line(Vec2(12, 16), Vec2(8, 8))) mustEqual true
        (r includes Line(Vec2(8, 16), Vec2(12, 16))) mustEqual false
        (r includes Line(Vec2(12, 10), Vec2(13, 10))) mustEqual false
      }
    }
  }

  "Algorithms" - {
    "LineIntersection" - {
      "Segments" in {
        val a = Line(Vec2(1, 3), Vec2(4, 1))
        val b = Line(Vec2(2, 1), Vec2(3, 3))
        val i = (a intersect b).get
        i.pos mustEqual Vec2(2.5, 2)
        i.onLine1 mustEqual true
        i.onLine2 mustEqual true
      }
      "SegmentAndLine" in {
        val a = Line(Vec2(1, 3), Vec2(4, 1))
        val b = Line(Vec2(0, 0), Vec2(2, 2))
        val i = (a intersect b).get
        i.onLine1 mustEqual true
        i.onLine2 mustEqual false
      }
      "LineAndSegment" in {
        val a = Line(Vec2(1, 3), Vec2(4, 1))
        val b = Line(Vec2(0, 3), Vec2(1, 4))
        val i = (a intersect b).get
        i.onLine1 mustEqual false
        i.onLine2 mustEqual true
      }
      "Parallel" in {
        val a = Line(Vec2(1, 3), Vec2(4, 1))
        val b = Line(Vec2(2, 3), Vec2(5, 1))
        val i = (a intersect b)
        i mustEqual None
      }
    }

    "FirstLineRectIntersection" - {
      "Intersect" in {
        val r = AARect.fromCenter(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(3, 2), Vec2(3, 3.5))
        val i = (r intersect l).right.get
        val i2 = (l intersect r).right.get
        i mustEqual Seq(Vec2(3, 3))
        i2 mustEqual Seq(Vec2(3, 3))
      }
      "NoIntersectInside" in {
        val r = AARect.fromCenter(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(2.5, 3.5), Vec2(3.5, 3.5))
        val i = (r intersect l).left.get
        i mustEqual true
      }
      "NoIntersectOutside" in {
        val r = AARect.fromCenter(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(2.5, 2.5), Vec2(2.5, 2.5))
        val i = (r intersect l).left.get
        i mustEqual false
      }
    }

    "LineCircleIntersection" - {
      "No intersection" in {
        val c = Circle(Vec2(1.5, 2.5), r = 1.5)
        val l = Line(Vec2(2, 0), Vec2(4, 1))
        val i1 = (c intersect l)
        val i2 = (l intersect c)
        val expected = Array()
        i1 mustEqual expected
        i2 mustEqual expected
      }
      "One intersection (tangent)" in {
        val c = Circle(Vec2(1.5, 2.5), r = 1.5)
        val l = Line(Vec2(5, 1), Vec2(6, 1))
        val i1 = (c intersect l)
        val i2 = (l intersect c)
        val expected = Array(Vec2(1.5, 1))
        i1 mustEqual expected
        i2 mustEqual expected
      }
      "Two intersections" in {
        val c = Circle(Vec2(1.5, 2.5), r = 1.5)
        val l = Line(Vec2(1.5, 5), Vec2(1.5, 2))
        val i1 = (c intersect l)
        val i2 = (l intersect c)
        val expected = Array(Vec2(1.5, 4), Vec2(1.5, 1))
        i1 mustEqual expected
        i2 mustEqual expected
      }

      "Two intersections (different order)" in {
        val c = Circle(Vec2(1.5, 2.5), r = 1.5)
        val l = Line(Vec2(1.5, 2), Vec2(1.5, 5))
        val i1 = (c intersect l)
        val i2 = (l intersect c)
        val expected = Array(Vec2(1.5, 1), Vec2(1.5, 4))
        i1 mustEqual expected
        i2 mustEqual expected
      }
    }

    "Circle includes other Circle" - {
      "bigger includes smaller, same center" in {
        val c1 = Circle(Vec2(1.5, 2.5), r = 2.5)
        val c2 = Circle(Vec2(1.5, 2.5), r = 1.5)
        (c1 includes c2) mustBe true
        (c2 includes c1) mustBe false
      }
      "bigger includes smaller, different center" in {
        val c1 = Circle(Vec2(1.5, 2.5), r = 2.5)
        val c2 = Circle(Vec2(1.3, 2.4), r = 0.5)
        (c1 includes c2) mustBe true
        (c2 includes c1) mustBe false
      }
      "bigger includes smaller, almost touching" in {
        val c1 = Circle(Vec2(51, 25), 36)
        val c2 = Circle(Vec2(14, -13), 91)
        (c1 includes c2) mustBe false
        (c2 includes c1) mustBe true
      }
      "overlapping, different center, same radius" in {
        val c1 = Circle(Vec2(1.5, 2.5), r = 2.5)
        val c2 = Circle(Vec2(1.3, 2.4), r = 2.5)
        (c1 includes c2) mustBe false
        (c2 includes c1) mustBe false
      }
      "identical circles" in {
        val c1 = Circle(Vec2(1.5, 2.5), r = 2.5)
        val c2 = Circle(Vec2(1.5, 2.5), r = 2.5)
        (c1 includes c2) mustBe true
        (c2 includes c1) mustBe true
      }
    }

    "Outer tangent of circles (clockwise)" - {
      "two different circles" in {
        val c1 = Circle(Vec2(1.5, 2.5), r = 1.5)
        val c2 = Circle(Vec2(2.5, 2.5), r = 1.5)
        val t1 = (c1 outerTangentCW c2)
        val t2 = (c2 outerTangentCW c1)
        t2 mustEqual Some(Line(Vec2(2.4999999999999996, 1), Vec2(1.4999999999999998, 1)))
        t1 mustEqual Some(Line(Vec2(1.5, 4), Vec2(2.5, 4)))
      }
      "circles, containing each other" in {
        val c1 = Circle(Vec2(-9, 17), 122)
        val c2 = Circle(Vec2(-9, 18), 45)
        val t1 = (c1 outerTangentCW c2)
        val t2 = (c2 outerTangentCW c1)
        t1 mustEqual None
        t2 mustEqual None
      }
      "two identical circles" in {
        val c1 = Circle(Vec2(2.5, 2.5), r = 1.5)
        val c2 = Circle(Vec2(2.5, 2.5), r = 1.5)
        val t1 = (c1 outerTangentCW c2)
        val t2 = (c2 outerTangentCW c1)
        t1 mustEqual None
        t2 mustEqual None
      }
    }

    "Outer tangent of circles (counter-clockwise)" - {
      "two different circles" in {
        val c1 = Circle(Vec2(1.5, 2.5), r = 1.5)
        val c2 = Circle(Vec2(2.5, 2.5), r = 1.5)
        val t1 = (c1 outerTangentCCW c2)
        val t2 = (c2 outerTangentCCW c1)
        t1 mustEqual Some(Line(Vec2(1.4999999999999998, 1), Vec2(2.4999999999999996, 1)))
        t2 mustEqual Some(Line(Vec2(2.5, 4), Vec2(1.5, 4)))
      }
      "circles, containing each other" in {
        val c1 = Circle(Vec2(-9, 17), 122)
        val c2 = Circle(Vec2(-9, 18), 45)
        val t1 = (c1 outerTangentCCW c2)
        val t2 = (c2 outerTangentCCW c1)
        t1 mustEqual None
        t2 mustEqual None
      }
      "two identical circles" in {
        val c1 = Circle(Vec2(2.5, 2.5), r = 1.5)
        val c2 = Circle(Vec2(2.5, 2.5), r = 1.5)
        val t1 = (c1 outerTangentCCW c2)
        val t2 = (c2 outerTangentCCW c1)
        t1 mustEqual None
        t2 mustEqual None
      }
    }

    "Circle AARect intersection" - {
      "not intersecting" in {
        val c = Circle(Vec2(3, 4), 2)
        val r = AARect.fromCenter(Vec2(30, 50), Vec2(20, 5))
        (c intersects r) mustEqual false
        (r intersects c) mustEqual false
      }

      "rect completely inside over circle center" in {
        val c = Circle(Vec2(3, 4), 20)
        val r = AARect.fromCenter(Vec2(2, 3), Vec2(4, 5))
        (c intersects r) mustEqual true
        (r intersects c) mustEqual true
      }

      "rect completely inside not over circle center" in {
        val c = Circle(Vec2(3, 4), 20)
        val r = AARect.fromCenter(Vec2(-3, 3), Vec2(2, 6))
        (c intersects r) mustEqual true
        (r intersects c) mustEqual true
      }

      "circle completely inside rect" in {
        val c = Circle(Vec2(3, 4), 2)
        val r = AARect.fromCenter(Vec2(1, 1), Vec2(8, 9))
        (c intersects r) mustEqual true
        (r intersects c) mustEqual true
      }

      "overlapping: circle center inside rect" in {
        val c = Circle(Vec2(3, 1), 2)
        val r = AARect.fromCenter(Vec2(1, 1), Vec2(6, 4))
        (c intersects r) mustEqual true
        (r intersects c) mustEqual true
      }

      "overlapping: circle center outside rect" in {
        val c = Circle(Vec2(5, 1), 2)
        val r = AARect.fromCenter(Vec2(1, 1), Vec2(6, 4))
        (c intersects r) mustEqual true
        (r intersects c) mustEqual true
      }

      "overlapping: circle center close to corner inside" in {
        val c = Circle(Vec2(3, 3), 4)
        val r = AARect.fromCenter(Vec2(-9, -9), Vec2(20, 20)) // top right corner at (1,1)
        (c intersects r) mustEqual true
        (r intersects c) mustEqual true
      }

      "overlapping: circle center close to corner outside" in {
        val c = Circle(Vec2(4, 4), 4)
        val r = AARect.fromCenter(Vec2(-9, -9), Vec2(20, 20)) // top right corner at (1,1)
        (c intersects r) mustEqual false
        (r intersects c) mustEqual false
      }
    }

    "CutLineByRect" - {
      // assuming there is only one intersection
      // which means that one line end is inside the rect
      // if there are two intersections the resulting line
      // can be wrong
      "Intersect" in {
        val r = AARect.fromCenter(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(3, 2), Vec2(3, 3.5))
        val c = (l cutBy r).get
        c mustEqual Line(Vec2(3, 2), Vec2(3, 3))
      }
      "NoCut" in {
        val r = AARect.fromCenter(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(3, 2), Vec2(4, 2))
        val c = (l cutBy r).get
        c mustEqual l
      }
      "FullCut" in {
        val r = AARect.fromCenter(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(2.5, 3.5), Vec2(3.5, 3.5))
        val c = (l cutBy r)
        c mustEqual None
      }
    }

    "ClampLineByRect" - {
      "Inside" in {
        val r = AARect.fromCenter(Vec2(4, 5), Vec2(4, 4))
        val l = Line(Vec2(3, 4), Vec2(4, 5))
        val c = (l clampBy r).get
        c mustEqual l
      }

      "StartOutside" in {
        val r = AARect.fromCenter(Vec2(4, 5), Vec2(4, 4))
        val l = Line(Vec2(1, 4), Vec2(4, 4))
        val c = (l clampBy r).get
        c mustEqual Line(Vec2(2, 4), Vec2(4, 4))
      }

      "StartInside" in {
        val r = AARect.fromCenter(Vec2(4, 5), Vec2(4, 4))
        val l = Line(Vec2(4, 4), Vec2(1, 4))
        val c = (l clampBy r).get
        c mustEqual Line(Vec2(4, 4), Vec2(2, 4))
      }

      "Outside" in {
        val r = AARect.fromCenter(Vec2(4, 5), Vec2(4, 4))
        val l = Line(Vec2(1, 2), Vec2(-1, 4))
        val c = (l clampBy r)
        c mustEqual None
      }

      "GoingThrough" in {
        val r = AARect.fromCenter(Vec2(4, 5), Vec2(4, 4))
        val l = Line(Vec2(1, 4), Vec2(7, 4))
        val c = (l clampBy r).get
        c mustEqual Line(Vec2(2, 4), Vec2(6, 4))
      }
    }
  }
}
