# vectory

A 2D geometry library in Scala.

## Dependency

For Scala and ScalaJS:

With [JitPack](https://jitpack.io), it is common to point to a specific commit to make your builds reproducible:

```scala
libraryDependencies += "com.github.fdietze.vectory" %% "vectory" % "76af304"
```

In ammonite you can use jitpack with a special resolver like this:

```scala
import $repo.`https://jitpack.io`
import $ivy.`com.github.fdietze.vectory::vectory:76af304`
```

## Example

```scala
import vectory._

Vec2(1,2) + Vec2(3,4) // Vec2(4.0,6.0)
Vec2(1,2).length // 2.23606797749979
Vec2(1,2).normalized // Vec2(0.4472135954999579,0.8944271909999159)
Vec2(1,2).angle // 1.1071487177940904
Vec2(1,2) dot Vec2(3,4) // 11.0
Vec2(1,2) cross Vec2(3,4) // -2.0
Vec2(1,2) * 5 // Vec2(5.0,10.0)

val a = Line(Vec2(1, 3), Vec2(4, 1))
val b = Line(Vec2(2, 1), Vec2(3, 3))
a intersect b // Some(LineIntersection(Vec2(2.5,2.0),true,true)) -- point lies on both segments

val c = Circle(Vec2(3, 1), 2)
val r = AARect(Vec2(1, 1), Vec2(6, 4))
c intersects r // true

val p = ConvexPolygon(IndexedSeq(Vec2(-3, 1), Vec2(-1, -2), Vec2(2, 2)))
val c = Circle(Vec2(-1, -4), 3)
p intersectsMtd c // Some(Vec2(-0.0,-1.0))

val p = ConvexPolygon(Array(Vec2(-2, -2), Vec2(-3, -1), Vec2(2, -2), Vec2(1, 3)))
p.aabb // AARect(Vec2(-0.5,0.5),Vec2(5.0,5.0))
```

## Primitives

* `Vec2(x: Double, y: Double)`
* `LineLine(start: Vec2, end: Vec2)`
* `Circle(center: Vec2, r: Double)`
* `AARect(center: Vec2, size: Vec2)` - Axis aligned rectangle
* `RotatedRect(center: Vec2, size: Vec2, angle: Double)` - Rotated rectangle
* `ConvexPolygon(verticesCCW: IndexedSeq[Vec2])` - Convex polygon with vertices in counter-clockwise order

## Algorithms
* Point-Line distance
* Point-Line-Segment distance
* Project point on line
* Axis-aligned bounding-box for a set of vertices
* Line-Line intersection returning intersection points
* Line-ConvexPolygon intersection returning intersection points
* Circle-AARect intersection returning intersection points
* Circle-ConvexPolygon intersection test
* Circle-ConvexPolygon intersection returning MTD (Minimum Translation Distance) vector
* ConvexPolygon-ConvexPolygon intersection returning MTD (Minimum Translation Distance) vector
* Clamp Line by ConvexPolygon
* Convex Hull of Points
