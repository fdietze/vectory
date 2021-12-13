package vectory

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Mat2dSpec extends AnyFreeSpec with Matchers {
  "constructor" in {
    val m = Mat2d(5, 7, 3, 1, 4, 9)
    m.m00 mustEqual 5
    m.m01 mustEqual 7
    m.m02 mustEqual 3
    m.m10 mustEqual 1
    m.m11 mustEqual 4
    m.m12 mustEqual 9
  }
  "addition" in {
    val a = Mat2d(4, 6, 3, 5, 1, 2)
    val b = Mat2d(6, 2, 1, 4, 5, 3)
    val c = a + b
    c mustEqual Mat2d(10, 8, 4, 9, 6, 5)
  }
  "subtraction" in {
    val a = Mat2d(3, 2, 6, 1, 5, 4)
    val b = Mat2d(4, 1, 2, 5, 3, 6)
    val c = a - b
    c mustEqual Mat2d(-1, 1, 4, -4, 2, -2)
  }
  "scalar multiplication" in {
    val a = Mat2d(5, 6, 1, 4, 2, 3)
    val c = a * 3
    c mustEqual Mat2d(15, 18, 3, 12, 6, 9)
  }
  "vector multiplication" in {
    val a = Mat2d(5, 6, 1, 4, 2, 3)
    val b = Vec2(7, 8)
    val c = a * b
    c mustEqual Vec2(84, 47)
  }
  "multiplication" in {
    val a = Mat2d(6, 5, 1, 4, 3, 2)
    val b = Mat2d(2, 4, 5, 3, 1, 6)

    val c = a * b
    c mustEqual Mat2d(27, 29, 61, 17, 19, 40)
  }
  "itentity" in {
    val m = Mat2d.identity
    m.m00 mustEqual 1
    m.m01 mustEqual 0
    m.m02 mustEqual 0
    m.m10 mustEqual 0
    m.m11 mustEqual 1
    m.m12 mustEqual 0
  }
  "scale" in {
    val m = Mat2d.scale2d(3, 5)
    m.m00 mustEqual 3
    m.m01 mustEqual 0
    m.m02 mustEqual 0
    m.m10 mustEqual 0
    m.m11 mustEqual 5
    m.m12 mustEqual 0
  }
  "scale from vec2" in {
    val m = Mat2d.scale2d(Vec2(3, 5))
    m.m00 mustEqual 3
    m.m01 mustEqual 0
    m.m02 mustEqual 0
    m.m10 mustEqual 0
    m.m11 mustEqual 5
    m.m12 mustEqual 0
  }
  "rotate 2d" in {
    val m = Mat2d.rotate2d(.25)
    m.m00 mustEqual (.968d +- .005)
    m.m01 mustEqual (-.247d +- .005)
    m.m02 mustEqual 0
    m.m10 mustEqual (.247d +- .005)
    m.m11 mustEqual (.968d +- .005)
    m.m12 mustEqual 0
  }
}
