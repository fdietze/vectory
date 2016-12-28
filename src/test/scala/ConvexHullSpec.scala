package vectory

import org.specs2.mutable.Specification

class ConvexHullSpec extends Specification {
  "test1" >> {
    val data = List(Vec2(0, 3), Vec2(2, 3), Vec2(3, 1), Vec2(2, 1))
    val dataSorted = List(Vec2(0, 3), Vec2(2, 1), Vec2(2, 3), Vec2(3, 1))
    val hull = List(Vec2(2, 3), Vec2(3, 1), Vec2(2, 1), Vec2(0, 3))

    dataSorted mustEqual data.sortWith(ConvexHull2D.compare)
    hull mustEqual ConvexHull2D(data)
  }

  "test2" >> {
    val data = List(Vec2(0, 3), Vec2(1, 0), Vec2(2, 1), Vec2(3, 0), Vec2(4, 3))
    val hull = List(Vec2(4.0, 3.0), Vec2(3.0, 0.0), Vec2(1.0, 0.0), Vec2(0.0, 3.0))
    hull mustEqual ConvexHull2D(data)
  }

  "test3" >> {
    val data = List(Vec2(0, 0), Vec2(0, 1), Vec2(0, 2), Vec2(0, 3))
    val hull = List(Vec2(0, 3), Vec2(0, 0))
    hull mustEqual ConvexHull2D(data)
  }
}
