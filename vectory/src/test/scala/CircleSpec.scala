package vectory

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class CircleSpec extends AnyFreeSpec with Matchers {
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
    c.sampleCircumference(4).toList mustEqual List(
      Vec2(7.0, 3.0),
      Vec2(2.0000000000000004, 8.0),
      Vec2(-3.0, 3.0000000000000004),
      Vec2(1.9999999999999991, -2.0),
    )
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
      val p = ConvexPolygon(
        Vec2Array(
          Vec2(268.3136177218223, 146.00249872072908),
          Vec2(266.93449214067226, 139.0691662220126),
          Vec2(263.0070747651478, 133.19137074541013),
          Vec2(257.1292792885453, 129.26395336988566),
          Vec2(-255.62455212282495, -206.02867772286498),
          Vec2(-271.6207653711504, -209.21052236919925),
          Vec2(-287.6169786194759, -206.02867772286498),
          Vec2(-301.1779134067779, -196.96755078777306),
          Vec2(-310.2390403418699, -183.40661600047096),
          Vec2(-313.42088498820414, -167.41040275214553),
          Vec2(-320.774494668978, 139.92789248579257),
          Vec2(-319.3953692000187, 146.86122442048787),
          Vec2(-315.46795214398657, 152.73901941893632),
          Vec2(-309.5901571455381, 156.66643647496846),
          Vec2(-302.65682521084284, 158.04556194392768),
          Vec2(250.19594678982887, 164.12016965272252),
          Vec2(257.1292792885453, 162.7410440715725),
          Vec2(263.0070747651478, 158.81362669604803),
          Vec2(266.9344921406723, 152.93583121944553),
        ),
      )
      val c = Circle(Vec2(-58.538346206308354, 134.5704658111136), 168.11767240585183)
      (p intersects c) mustEqual true
      (c intersects p) mustEqual true
    }
  }
}
