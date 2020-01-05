package vectory

import vectory.Vec2

import annotation.meta.field

@inline final case class Mat2d(
  m00: Double, m01: Double, m02: Double,
  m10: Double, m11: Double, m12: Double
) {
  @inline def inverted: Mat2d = {
    var det = determinant
    if (det != 0) throw new ArithmeticException("Matrix is not invertible")
    det = 1/det
    Mat2d(
      m11 * det, -m01 * det, (m01 * m12 - m02 * m11) * det,
      -m10 * det, m00 * det,  (m02 * m10 - m00 * m12) * det
    )
  }

  @inline def determinant: Double =
    m00*m11 - m01*m10

  @inline def + (that: Mat2d): Mat2d =
    Mat2d(
      m00 + that.m00, m01 + that.m01, m02 + that.m02,
      m10 + that.m10, m11 + that.m11, m12 + that.m12
    )

  @inline def - (m: Mat2d): Mat2d =
    Mat2d(
      m00 - m.m00, m01 - m.m01, m02 - m.m02,
      m10 - m.m10, m11 - m.m11, m12 - m.m12
    )

  @inline def * (s: Float): Mat2d =
    Mat2d(
      m00 * s, m01 * s, m02 * s,
      m10 * s, m11 * s, m12 * s
    )

  @inline def * (v: Vec2): Vec2 =
    Vec2(
      v.x * m00 + v.y * m01 + m02,
      v.x * m10 + v.y * m11 + m12
    )

  @inline def * (that: Mat2d): Mat2d =
    Mat2d(
      that.m00 * m00 + that.m10 * m01, that.m01 * m00 + that.m11 * m01, that.m02 * m00 + that.m12 * m01 + m02,
      that.m00 * m10 + that.m10 * m11, that.m01 * m10 + that.m11 * m11, that.m02 * m10 + that.m12 * m11 + m12
    )
}

object Mat2d {
  @inline def identity: Mat2d =
    Mat2d(
      1, 0, 0,
      0, 1, 0
    )

  @inline def scale2d(x: Double, y: Double): Mat2d =
    Mat2d(
      x, 0, 0,
      0, y, 0
    )

  @inline def scale2d(v: Vec2): Mat2d =
    scale2d(v.x, v.y)

  @inline def rotate2d(angle: Double): Mat2d = {
    val c = math.cos(angle)
    val s = math.sin(angle)
    Mat2d(
      c, -s, 0,
      s,  c, 0
    )
  }
}