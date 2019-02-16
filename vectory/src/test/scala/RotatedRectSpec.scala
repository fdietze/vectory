package vectory

import org.scalatest._

class RotatedRectSpec extends FreeSpec with MustMatchers {
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
