package vectory

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

// online plotting tool: https://www.math10.com/en/geometry/geogebra/geogebra.html

class ConvexHullOfCircleSpec extends AnyFreeSpec with Matchers {
  "empty" in {
    val circles = List.empty[Circle]
    val hull    = ConvexHullOfCircles(circles)
    hull.toList mustEqual List.empty[Circle]
  }

  "one circle" in {
    val c1      = Circle(Vec2(2, 3), 5)
    val circles = List(c1)
    val hull    = ConvexHullOfCircles(circles)
    hull.toList mustEqual List(c1)
  }

  "2 circles" in {
    val c1      = Circle(Vec2(2, 3), 5)
    val c2      = Circle(Vec2(9, 0), 2)
    val circles = List(c1, c2)
    val hull    = ConvexHullOfCircles(circles)
    hull.toList mustEqual List(c2, c1)
  }

  "2 circles: c1 completely containing c2" in {
    val c1      = Circle(Vec2(2, 3), 5)
    val c2      = Circle(Vec2(2, 1), 2)
    val circles = List(c1, c2)
    val hull    = ConvexHullOfCircles(circles)
    hull.toList mustEqual List(c1)
  }

  "2 circles: c1 identical to c2" in {
    val c1      = Circle(Vec2(2, 3), 5)
    val c2      = Circle(Vec2(2, 3), 5)
    val circles = List(c1, c2)
    val hull    = ConvexHullOfCircles(circles)
    hull.toList mustEqual List(c1)
  }

  "3 circles: c3 inside hull of c1 and c2" in {
    val c1      = Circle(Vec2(2, 3), 5)
    val c2      = Circle(Vec2(11, -2), 6)
    val c3      = Circle(Vec2(8, 0), 2)
    val circles = List(c1, c2, c3)
    val hull    = ConvexHullOfCircles(circles)
    hull.toList mustEqual List(c2, c1)
  }

  "3 circles: c3 appears on hull twice" in {
    val c1      = Circle(Vec2(-7, 5), 1)
    val c2      = Circle(Vec2(2, 4), 3)
    val c3      = Circle(Vec2(-3, 5), 4)
    val circles = List(c1, c2, c3)
    val hull    = ConvexHullOfCircles(circles)
    hull.toList mustEqual List(c3, c2, c3, c1)
  }

  "3 circles: sampling instability, inner touching outer" in {
    val c1      = Circle(Vec2(51, 25), 36)
    val c2      = Circle(Vec2(14, -13), 91)
    assert(c2 includes c1)
    val circles = List(c1, c2)
    val hull    = ConvexHullOfCircles(circles)
    hull.toList mustEqual List(c2)
  }

  "3 circles: counter clockwise order" in {
    val c1      = Circle(Vec2(95, 46), 36)
    val c2      = Circle(Vec2(80, -20), 15)
    val c3      = Circle(Vec2(1, 29), 51)
    val circles = List(c1, c2, c3) // input is cw
    val hull    = ConvexHullOfCircles(circles)
    hull.toList mustEqual List(c2, c1, c3)
  }
}
