package vectory

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class AARectSpec extends AnyFreeSpec with Matchers {
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
