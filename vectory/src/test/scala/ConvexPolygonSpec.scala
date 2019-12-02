package vectory

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ConvexPolygonSpec extends AnyFreeSpec with Matchers {
  "point inside" in {
    val p = ConvexPolygon(Vec2Array(Vec2(-3, 1), Vec2(-1, -4), Vec2(3, -5), Vec2(5, -1), Vec2(2, 2)))
    val c = Circle(Vec2(2, -2), 2)
    p.includes(c.center) mustEqual true
  }

  "axis aligned bounding box" in {
    val p = ConvexPolygon(Vec2Array(Vec2(-2, -2), Vec2(-3, -1), Vec2(2, -2), Vec2(1, 3)))
    p.aabb mustEqual AARect.fromCenter(Vec2(-0.5, 0.5), Vec2(5, 5))
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
}
