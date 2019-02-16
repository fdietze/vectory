package vectory

import org.scalatest._

class AlgorithmSpec extends FreeSpec with MustMatchers {
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
