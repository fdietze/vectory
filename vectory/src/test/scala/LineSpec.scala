package vectory

import org.scalatest._

class LineSpec extends FreeSpec with MustMatchers {
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
