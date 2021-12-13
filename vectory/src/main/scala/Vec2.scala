package vectory

import annotation.meta.field

import flatland._

@inline final case class Vec2(x: Double, y: Double) {
  @inline def width  = x
  @inline def height = y

  @inline def unary_- = Vec2(-x, -y)
  @inline def abs     = Vec2(Math.abs(x), Math.abs(y))

  @inline def +(that: Vec2)     = Vec2(this.x + that.x, this.y + that.y)
  @inline def +(that: Double)   = Vec2(this.x + that, this.y + that)
  @inline def -(that: Vec2)     = Vec2(this.x - that.x, this.y - that.y)
  @inline def -(that: Double)   = Vec2(this.x - that, this.y - that)
  @inline def *(a: Double)      = Vec2(this.x * a, this.y * a)
  @inline def /(a: Double)      = Vec2(this.x / a, this.y / a)
  @inline def dot(that: Vec2)   = this.x * that.x + this.y * that.y
  @inline def cross(that: Vec2) = this.x * that.y - this.y * that.x

  @inline def lengthSq   = x * x + y * y
  @inline def length     = Math.sqrt(lengthSq)
  @inline def normalized = this / length
  @inline def area       = x * y
  @inline def normal     = Vec2(y, -x)

  @inline def angle = Math.atan2(y, x)

  @inline def toTuple                = (x, y)
  @inline def toArray: Array[Double] = {
    val a = new Array[Double](2)
    a(0) = x
    a(1) = y
    a
  }
}

object Vec2 {
  @inline def apply(tuple: (Double, Double)) = new Vec2(tuple._1, tuple._2)
  @inline def apply(x: Double)               = new Vec2(x, x)

  @inline def zero                = new Vec2(0, 0)
  @inline def unitX               = new Vec2(1, 0)
  @inline def unitY               = new Vec2(0, 1)
  @inline def unit(angle: Double) = Vec2(Math.cos(angle), Math.sin(angle))

  @inline def dot(x1: Double, y1: Double, x2: Double, y2: Double) = x1 * x2 + y1 * y2
  @inline def lengthSq(x: Double, y: Double)                      = x * x + y * y
  @inline def length(x: Double, y: Double)                        = Math.sqrt(lengthSq(x, y))
  @inline def normalize(length: Double, component: Double)        = component / length
}
