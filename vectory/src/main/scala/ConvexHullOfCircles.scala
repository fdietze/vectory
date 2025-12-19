package vectory

import collection.mutable

object ConvexHullOfCircles {
  def apply(circles: Seq[Circle]): Seq[Circle] = ConvexHullOfCirclesSampling(circles)
}

object ConvexHullOfCirclesSampling {
  def compress(seq: Seq[Circle]): Seq[Circle] = {
    if (seq.isEmpty) return seq

    def including(a: Circle, b: Circle) = a == b || (a includes b) || (b includes a)

    // we also remove consequtive circles which include each other.
    // This can happen, because of sampling inaccuracy
    val compressed = seq.head :: seq.sliding(2).collect { case Seq(a, b) if !including(a, b) => b }.toList
    if (compressed.size > 2 && including(compressed.head, compressed.last)) compressed.init else compressed
  }

  def apply(circles: Seq[Circle]): Seq[Circle] = {
    val samples                          = 10
    val pointToCircle: Map[Vec2, Circle] = circles.flatMap { c => c.sampleCircumference(samples).map(_ -> c) }.toMap
    val hull                             = compress(ConvexHull2D(pointToCircle.keys).map(pointToCircle))
    // if (hull.size > 1) (hull :+ hull.head).sliding(2).foreach{ case Seq(c1, c2) => assert(!c1.includes(c2), s"$c1 includes $c2"); assert(!c2.includes(c1), s"$c2 includes $c1") }
    hull.reverse
  }
}

// Algorithm from paper
// A convex hull algorithm for discs, and applications (David Rappaport)
// also inspired by C++ implementation https://github.com/nsub93/convex-hull-disks

// object ConvexHullOfCircles {
//   val lStarDefault = Line(Vec2(0,0), Vec2(0,100))

//   object CH {
//     def apply(circles:Iterable[Circle] = Nil) = new CH(circles.toBuffer)
//   }
//   class CH(val circles:mutable.Buffer[Circle] = mutable.ArrayBuffer[Circle]()) extends AnyVal {
//   }

//   def merge(chP:CH, chQ:CH):CH = {
//     val p = chP.circles.head
//     val q = chQ.circles.head
//     var lStar = lStarDefault
//     var lp = parallel_support_line(p, lStar)
//     var lq = parallel_support_line(q, lStar)
//     var chS = CH()

//     @inline def everyArcInPandQHasBeenVisited:Boolean = ???

//     do {
//       if(dom(lp,lq)) {
//         add(chS, p)
//         advance(p,q) match {
//           case (newp,newq) =>
//             p = newp
//             q = newq
//         }
//       } else {
//         add(chS, q)
//         advance(q,p) match {
//           case (newq,newp) =>
//             q = newq
//             p = newp
//         }
//       }
//       lp = parallel_support_line(p, lStar)
//       lq = parallel_support_line(q, lStar)
//     } while (everyArcInPandQHasBeenVisited)

//     def advance(x:Circle,xCh:CH, y:Circle, yCh:CH):(Circle,Circle) = {
//       // if L(x,y) does not exist...
//       val a1 = alpha(lStar, common_support_line(x,y))
//       // val a2 = alpha(lStar, )
//       ???
//     }

//     chS
//   }

//   def hull(in: Seq[Circle]): CH = {
//     if(in.size <= 1) return CH(in)

//     val p = in.take(in.length/2)
//     val q = in.drop(in.length/2)
//    merge(hull(p), hull(q))
//   }

//   def parallel_support_line(c:Circle, lStar:Line):Line = {
//     val n = lStar.normal.normalized
//     val p = c.center - n*c.r
//     val od = Line(p, c.center).normal
//     Line(Vec2(0,0), Vec2(od.y, od.x))
//   }

//   def alpha(l1:Line, l2:Line):Double = ???

//   def dom(l1:Line, l2:Line): Boolean = ???

//   def add(theta: CH, phi:Circle):Unit = {
//     if(theta.circles.last != phi)
//       theta.circles += phi
//   }

//   def succ(ch:CH, c:Circle):Circle = {
//     if(ch.circles.size == 1) return ch.circles.head
//     ch.circles((ch.circles.indexOf(c)+1) % ch.circles.size)
//   }
// }

// from paper: incremental algorithms for finding the convex hulls of circles and lower envelopes of parabolas
// by Olivier Devilles and Mordecai Golin

object ConvexHullOfCircles2 {
  case class Arc(circle: Circle, start: Double = 0, end: Double = 2 * Math.PI) {
    val startPos = circle.center + (Vec2.unit(start) * circle.r)
    val endPos   = circle.center + (Vec2.unit(end) * circle.r)
  }

  case class ArcLeaf(arc: Arc, q: Vec2) {
    val startAngle = Line(q, arc.startPos).vector.angle
    val endAngle   = Line(q, arc.endPos).vector.angle
  }

  class Arcs(val q: Vec2, var arcs: mutable.ArrayBuffer[ArcLeaf] = mutable.ArrayBuffer.empty[ArcLeaf]) {
    def isEmpty            = arcs.isEmpty
    // arcs are stored in clockwise order
    def +=(arc: Arc): Unit = {
      arcs += ArcLeaf(arc, q)
    }

    def cutOut(p: Vec2, circle: Circle) = {}

    def findLeftArc(angle: Double): ArcLeaf = {
      // TODO: wrap around, modulo
      arcs.filter(_.endAngle < angle).maxBy(_.endAngle)
    }

    def findRightArc(angle: Double): ArcLeaf = {
      // TODO: wrap around, modulo
      arcs.filter(_.startAngle > angle).minBy(_.startAngle)
    }

    def calculate_p(p_prime: Vec2): Option[Vec2] = {
      val ray                  = Line(q, p_prime)
      val angle                = ray.vector.angle
      val arcOpt               = arcs.find(arcLeaf =>
        arcLeaf.startAngle <= angle && angle < arcLeaf.endAngle,
      ) // endAngle is exclusive //TODO: wrap around, modulo
      val p: Vec2              = arcOpt match {
        case Some(arcLeaf) => (ray intersect arcLeaf.arc.circle).last
        case None          =>
          // point does not lie on arc, but on line
          val line = Line(findLeftArc(angle).arc.endPos, findRightArc(angle).arc.startPos)
          (ray intersect line).get.pos
      }
      val pIsBetweenQandPPrime = Line(q, p).lengthSq < Line(q, p_prime).lengthSq
      if (pIsBetweenQandPPrime) {
        // p does not lie on the new convex hull
        // new circle is neither competely inside nor intersects convex hull.
        Some(p)
      } else {
        // circle is not outside of the old convex hull
        // therefore either circle intersects the old hull or is totally contained by it
        None
      }
    }
  }

  class CH(q: Vec2) {
    val arcs                 = new Arcs(q)
    def add(c: Circle): Unit = {
      if (arcs.isEmpty) {
        arcs += Arc(c)
      } else {
        // circle must be smaller than all other circles contained in this hull
        // TODO: must p_prime be on the circle area or hull?
        val pOpt: Option[Vec2] = arcs.calculate_p(p_prime = c.center)
        pOpt match {
          case Some(p) => // new circle is outside
            // the current tangent or arc which contains p is A
            val a = ???
          // now walk clockwise to find A1, destroying all arcs traversed
          // and counterclockwise to find A2, destroying all arcs traversed
          case None    => // new circle intersects the hull or is completely inside
            ???
        }
      }
    }

    def circles = arcs.arcs.map(_.arc.circle)
  }

  def hull(circles: Seq[Circle]): collection.Seq[Circle] = {
    val sortedCircles = circles.sortBy(c => -c.r) // sort by radius, biggest first
    val q: Vec2       = sortedCircles.head.center
    val ch            = new CH(q)
    sortedCircles.foreach(circle => ch.add(circle))
    ch.circles
  }

}
