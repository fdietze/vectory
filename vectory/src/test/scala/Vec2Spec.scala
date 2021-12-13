package vectory

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class Vec2Spec extends AnyFreeSpec with Matchers {
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
