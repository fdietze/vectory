package vectory

import org.specs2.mutable.Specification

class GeometrySpec extends Specification {
  "Primitives" >> {
    "Vec2" >> {
      "constructor" >> {
        val v = Vec2(5, 7)
        v.x mustEqual 5
        v.y mustEqual 7
        v.width mustEqual 5
        v.height mustEqual 7
      }
      "from tuple" >> {
        val v = Vec2((5, 7))
        v.x mustEqual 5
        v.y mustEqual 7
      }
      "to tuple" >> {
        val v = Vec2(5, 7)
        v.toTuple mustEqual (5, 7)
      }
      "addition" >> {
        val a = Vec2(5, 7)
        val b = Vec2(2, 3)
        val c = a + b
        c.x mustEqual 7
        c.y mustEqual 10
      }
      "scalar addition" >> {
        val a = Vec2(5, 7)
        val b = 3
        val c = a + b
        c.x mustEqual 8
        c.y mustEqual 10
      }
      "substraction" >> {
        val a = Vec2(5, 7)
        val b = Vec2(2, 3)
        val c = a - b
        c.x mustEqual 3
        c.y mustEqual 4
      }
      "multiplication" >> {
        val a = Vec2(5, 7)
        val c = a * 3
        c.x mustEqual 15
        c.y mustEqual 21
      }
      "division" >> {
        val a = Vec2(6, 8)
        val c = a / 2
        c.x mustEqual 3
        c.y mustEqual 4
      }
      "crossProduct" >> {
        val a = Vec2(6, 8)
        val b = Vec2(2, 3)
        (a cross b) mustEqual 2
      }
      "dotProduct" >> {
        val a = Vec2(6, 8)
        val b = Vec2(2, 3)
        (a dot b) mustEqual 36
      }
      "length" >> {
        Vec2(3, 4).lengthSq mustEqual 25
        Vec2(3, 4).length mustEqual 5
      }
    }

    "Line" >> {
      "constructor" >> {
        val l = Line(Vec2(7, 3), Vec2(8, 4))
        l.start mustEqual Vec2(7, 3)
        l.end mustEqual Vec2(8, 4)
        l.x1 mustEqual 7
        l.y1 mustEqual 3
        l.x2 mustEqual 8
        l.y2 mustEqual 4
      }

      "vector" >> {
        val l = Line(Vec2(7, 3), Vec2(8, 5))
        l.vector mustEqual Vec2(1, 2)
      }

      "center" >> {
        val l = Line(Vec2(7, 3), Vec2(8, 5))
        l.center mustEqual Vec2(7.5, 4)
      }

      "angle" >> {
        val l = Line(Vec2(7, 3), Vec2(12, 3))
        l.vector.angle mustEqual 0
        val m = Line(Vec2(7, 3), Vec2(6, 3))
        m.vector.angle mustEqual Math.PI
        val n = Line(Vec2(7, 3), Vec2(7, 5))
        n.vector.angle mustEqual Math.PI / 2
        val o = Line(Vec2(7, 3), Vec2(7, 1))
        o.vector.angle mustEqual -Math.PI / 2
      }

      "rightOf" >> {
        val l = Line(Vec2(4, 12), Vec2(8, 0))
        val p = Vec2(12, 16)
        (l rightOf p) must beFalse
        (l leftOf p) must beTrue
      }

      "equals" >> {
        val l = Line(Vec2(7, 3), Vec2(8, 4))
        val m = Line(Vec2(8, 4), Vec2(7, 3))
        l mustEqual l
        l mustEqual m
      }

      "hashCode" >> {
        val l = Line(Vec2(7, 3), Vec2(8, 4))
        val m = Line(Vec2(8, 4), Vec2(7, 3))
        l.hashCode mustEqual m.hashCode
      }

      "length" >> {
        val l = Line(Vec2(5, 5), Vec2(8, 9))
        l.length mustEqual 5
      }
    }

    "AARect" >> {
      "constructor" >> {
        val r = AARect(Vec2(11, 5), Vec2(8, 4))
        r.pos mustEqual Vec2(11, 5)
        r.size mustEqual Vec2(8, 4)
        r.x mustEqual 11
        r.y mustEqual 5
        r.width mustEqual 8
        r.height mustEqual 4
      }

      "minMaxCorner" >> {
        val r = AARect(Vec2(11, 5), Vec2(8, 4))
        r.minCorner mustEqual Vec2(7, 3)
        r.maxCorner mustEqual Vec2(15, 7)
      }

      "corners" >> {
        val r = AARect(Vec2(3, 3.5), Vec2(2, 1))
        val c = r.corners.toList
        c.toList mustEqual List(Vec2(2, 3), Vec2(4, 3), Vec2(4, 4), Vec2(2, 4))
      }

      "edges" >> {
        val r = AARect(Vec2(3, 3.5), Vec2(2, 1))
        val e = r.edges.toList
        e mustEqual List(
          Line(Vec2(2, 3), Vec2(4, 3)),
          Line(Vec2(4, 3), Vec2(4, 4)),
          Line(Vec2(4, 4), Vec2(2, 4)),
          Line(Vec2(2, 4), Vec2(2, 3))
        )
      }

      "PointInside" >> {
        val r = AARect(Vec2(3, 3.5), Vec2(2, 1))
        (r includes Vec2(3, 3.5)) mustEqual true
        (r includes Vec2(3, 4.5)) mustEqual false
      }

      "LineInside" >> {
        val r = AARect(Vec2(3, 3.5), Vec2(2, 1))
        (r includes Line(Vec2(2.5, 3.5), Vec2(3.5, 3.5))) mustEqual true
        (r includes Line(Vec2(2.5, 3.5), Vec2(5.5, 3.5))) mustEqual false
        (r includes Line(Vec2(4.5, 3.5), Vec2(5.5, 3.5))) mustEqual false
      }
      "OverlappingRect" >> {
        val r1 = AARect(Vec2(2, 3), Vec2(4, 4))
        val r2 = AARect(Vec2(1, 4), Vec2(3, 1))
        val r3 = AARect(Vec2(10, 10), Vec2(1, 1))
        (r1 isOverlapping r2) must beTrue
        (r2 isOverlapping r1) must beTrue
        (r1 isOverlapping r3) must beFalse
        (r3 isOverlapping r1) must beFalse
      }
    }

    "RotatedRect" >> {
      "constructor" >> {
        val r = RotatedRect(Vec2(11, 5), Vec2(8, 4), Math.PI / 4)
        r.pos mustEqual Vec2(11, 5)
        r.size mustEqual Vec2(8, 4)
        r.x mustEqual 11
        r.y mustEqual 5
        r.width mustEqual 8
        r.height mustEqual 4
      }

      "minMaxCorner" >> {
        val r = RotatedRect(Vec2(8, 9.5), Vec2(20, 5), Math.atan(4.0 / 3.0))
        val roundedMin = Vec2(Math.round(r.minCorner.x), Math.round(r.minCorner.y))
        val roundedMax = Vec2(Math.round(r.maxCorner.x), Math.round(r.maxCorner.y))
        roundedMin mustEqual Vec2(4, 0)
        roundedMax mustEqual Vec2(12, 19)
      }

      "corners" >> {
        val r = RotatedRect(Vec2(8, 9.5), Vec2(20, 5), Math.atan(4.0 / 3.0))
        val c = r.corners.toList
        val rounded = c.toList.map(v => Vec2(Math.round(v.x), Math.round(v.y)))
        rounded mustEqual List(Vec2(4, 0), Vec2(0, 3), Vec2(12, 19), Vec2(16, 16))
      }

      "edges" >> {
        val r = RotatedRect(Vec2(8, 9.5), Vec2(20, 5), Math.atan(4.0 / 3.0))
        val e = r.edges.toList
        val rounded = e.toList.map(l => Line(
          Vec2(Math.round(l.start.x), Math.round(l.start.y)),
          Vec2(Math.round(l.end.x), Math.round(l.end.y))
        ))
        rounded mustEqual List(
          Line(Vec2(4, 0), Vec2(0, 3)),
          Line(Vec2(0, 3), Vec2(12, 19)),
          Line(Vec2(12, 19), Vec2(16, 16)),
          Line(Vec2(16, 16), Vec2(4, 0))
        )
      }
      "PointInside" >> {
        val r = RotatedRect(Vec2(8, 9.5), Vec2(20, 5), Math.atan(4.0 / 3.0))
        (r includes Vec2(4, 4)) mustEqual true
        (r includes Vec2(8, 12)) mustEqual true
        (r includes Vec2(12, 8)) mustEqual false
        (r includes Vec2(12, 10)) mustEqual false
      }

      "LineInside" >> {
        val r = RotatedRect(Vec2(8, 9.5), Vec2(20, 5), Math.atan(4.0 / 3.0))
        (r includes Line(Vec2(12, 16), Vec2(8, 8))) mustEqual true
        (r includes Line(Vec2(8, 16), Vec2(12, 16))) mustEqual false
        (r includes Line(Vec2(12, 10), Vec2(13, 10))) mustEqual false
      }
    }
  }
  "Algorithms" >> {
    "LineIntersection" >> {
      "Segments" >> {
        val a = Line(Vec2(1, 3), Vec2(4, 1))
        val b = Line(Vec2(2, 1), Vec2(3, 3))
        val i = (a intersect b).get
        i.pos mustEqual Vec2(2.5, 2)
        i.onLine1 mustEqual true
        i.onLine2 mustEqual true
      }
      "SegmentAndLine" >> {
        val a = Line(Vec2(1, 3), Vec2(4, 1))
        val b = Line(Vec2(0, 0), Vec2(2, 2))
        val i = (a intersect b).get
        i.onLine1 mustEqual true
        i.onLine2 mustEqual false
      }
      "LineAndSegment" >> {
        val a = Line(Vec2(1, 3), Vec2(4, 1))
        val b = Line(Vec2(0, 3), Vec2(1, 4))
        val i = (a intersect b).get
        i.onLine1 mustEqual false
        i.onLine2 mustEqual true
      }
      "Parallel" >> {
        val a = Line(Vec2(1, 3), Vec2(4, 1))
        val b = Line(Vec2(2, 3), Vec2(5, 1))
        val i = (a intersect b)
        i mustEqual None
      }
    }
    "FirstLineRectIntersection" >> {
      "Intersect" >> {
        val r = Rect(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(3, 2), Vec2(3, 3.5))
        val i = (r intersect l).right.get
        val i2 = (l intersect r).right.get
        i mustEqual Seq(Vec2(3, 3))
        i2 mustEqual Seq(Vec2(3, 3))
      }
      "NoIntersectInside" >> {
        val r = Rect(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(2.5, 3.5), Vec2(3.5, 3.5))
        val i = (r intersect l).left.get
        i mustEqual true
      }
      "NoIntersectOutside" >> {
        val r = Rect(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(2.5, 2.5), Vec2(2.5, 2.5))
        val i = (r intersect l).left.get
        i mustEqual false
      }
    }

    "CutLineByRect" >> {
      // assuming there is only one intersection
      // which means that one line end is inside the rect
      // if there are two intersections the resulting line
      // can be wrong
      "Intersect" >> {
        val r = Rect(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(3, 2), Vec2(3, 3.5))
        val c = (l cutBy r).get
        c mustEqual Line(Vec2(3, 2), Vec2(3, 3))
      }
      "NoCut" >> {
        val r = Rect(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(3, 2), Vec2(4, 2))
        val c = (l cutBy r).get
        c mustEqual l
      }
      "FullCut" >> {
        val r = Rect(Vec2(3, 3.5), Vec2(2, 1))
        val l = Line(Vec2(2.5, 3.5), Vec2(3.5, 3.5))
        val c = (l cutBy r)
        c mustEqual None
      }
    }

    "ClampLineByRect" >> {
      "Inside" >> {
        val r = Rect(Vec2(4, 5), Vec2(4, 4))
        val l = Line(Vec2(3, 4), Vec2(4, 5))
        val c = (l clampBy r).get
        c mustEqual l
      }

      "StartOutside" >> {
        val r = Rect(Vec2(4, 5), Vec2(4, 4))
        val l = Line(Vec2(1, 4), Vec2(4, 4))
        val c = (l clampBy r).get
        c mustEqual Line(Vec2(2, 4), Vec2(4, 4))
      }

      "StartInside" >> {
        val r = Rect(Vec2(4, 5), Vec2(4, 4))
        val l = Line(Vec2(4, 4), Vec2(1, 4))
        val c = (l clampBy r).get
        c mustEqual Line(Vec2(4, 4), Vec2(2, 4))
      }

      "Outside" >> {
        val r = Rect(Vec2(4, 5), Vec2(4, 4))
        val l = Line(Vec2(1, 2), Vec2(-1, 4))
        val c = (l clampBy r)
        c mustEqual None
      }

      "GoingThrough" >> {
        val r = Rect(Vec2(4, 5), Vec2(4, 4))
        val l = Line(Vec2(1, 4), Vec2(7, 4))
        val c = (l clampBy r).get
        c mustEqual Line(Vec2(2, 4), Vec2(6, 4))
      }
    }
  }
}
