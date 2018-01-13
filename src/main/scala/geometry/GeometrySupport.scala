package geometry

import optimum.OptimumParametrisedSearch

/**
  * Created by k.neyman on 20.11.2017.
  */
trait GeometrySupport {
  implicit class LineCrossDetector(val vector: MyVector) {

    private def determinant(a1: Double, b1: Double, a2: Double, b2: Double): Double = {
      a1 * b2 - a2 * b1
    }

    private def hasSamePoint(anotherVector: MyVector): Boolean = {
      vector.from == anotherVector.from ||
        vector.from == anotherVector.to ||
        vector.to == anotherVector.from ||
        vector.to == anotherVector.to
    }

    def findCrossing(anotherVector: MyVector): Option[Point] = {
      if (vector == anotherVector) {
        throw new IllegalArgumentException("Vectors are equals")
      }

      val D = determinant(vector.line.a, vector.line.b, anotherVector.line.a, anotherVector.line.b)
      def Dx = determinant(-vector.line.c, vector.line.b, -anotherVector.line.c, anotherVector.line.b)
      def Dy = determinant(vector.line.a, -vector.line.c, anotherVector.line.a, -anotherVector.line.c)

      if (D == 0) None
      else Some(Point(Dx / D, Dy / D))
        .filter(p => anotherVector.containsAsRectangle(p) && vector.containsAsRectangle(p))
    }

    def isCrossedBy(anotherVector: MyVector): Boolean = {
      findCrossing(anotherVector).isDefined
    }
  }

  implicit class FigureLineInterception(val figure: Figure) {
    private val EPS_ACCURACY_CROSS_DETECTION_AT_BOUND: Double = 0.1

    def findCrossings(vector: MyVector): Iterable[Point] = {
      val v1 = figure.vertex.filter(p =>
        vector.contains(p) &&
          vector.containsAsRectangle(p) &&
          checkPoint(p, vector))

      val v2 = figure.lines.flatMap(line =>
        line.findCrossing(vector) match {
          case optP: Some[Point] => optP.filter(checkPoint(_, vector))
          case _ => None
        })
      (v1 ::: v2).distinct
    }

    def checkPoint(p: Point, vector: MyVector): Boolean = {
      if (p == vector.from) isCrossingFromOutside(vector)
      else if (p == vector.to) isCrossingFromInside(vector)
      else true
    }

    def isCrossingFromOutside(vector: MyVector): Boolean = {
      figure.containsPoint(new ParametrizedLine(vector).apply(EPS_ACCURACY_CROSS_DETECTION_AT_BOUND))
    }

    def isCrossingFromInside(vector: MyVector): Boolean = {
      figure.containsPoint(new ParametrizedLine(vector).apply(1 - EPS_ACCURACY_CROSS_DETECTION_AT_BOUND))
    }
  }

  type OptFun = Point => Double
  implicit class MoveAlongFigure(val figure: Figure) {
    def moveAlongToTarget(from: Point, optimim: OptFun): List[MyVector] = {
      val edge = getInitEdge(from)

      val (left, (_ :: right)) = figure.lines.span(_ != edge)
      val circle = right ::: left

      val minLeft = moveRound(from, edge :: circle, optimim, reversed = false)
      val minRight = moveRound(from, edge :: circle.reverse, optimim, reversed = true)
      selectMinPath(from, minLeft, minRight)(optimim)
    }

    private def selectMinPath(from: Point,
                              pathLeft: List[MyVector],
                              pathRight: List[MyVector])
                             (implicit optimum: OptFun): List[MyVector] = {
      val initVal = optimum(from)
      (pathLeft, pathRight) match {
        case (Nil, Nil) => Nil
        case (Nil, _) =>
          if (result(pathRight) < initVal) optimise(pathRight)
          else Nil
        case (_, Nil) =>
          if (result(pathLeft) < initVal) optimise(pathLeft)
          else Nil
        case (_, _) =>
          val left: Double = result(pathLeft)
          val right: Double = result(pathRight)
          if (left < initVal || right < initVal) {
            if (left < right) optimise(pathLeft)
            else optimise(pathRight)
          } else {
            Nil
          }
      }
    }

    private def optimise(path: List[MyVector]): List[MyVector] = {
      path.filterNot(v => v.from == v.to)
    }

    private def result(list: List[MyVector])(implicit optimim: OptFun): Double = {
      optimim(list.last.to)
    }

    private def moveRound(from: Point, lines: List[MyVector], optimim: OptFun, reversed: Boolean): List[MyVector] = {
      val tValToGoNext = if (reversed) 0 else 1
      lines match {
        case head :: tail =>
          val paramLine = new ParametrizedLine(head)
          val tMin = moveAlongEdge(paramLine, optimim)
          val nextP = paramLine(tMin)
          if (tMin == tValToGoNext)
            MyVector(from, nextP) :: moveRound(nextP, tail, optimim, reversed)
          else
            MyVector(from, nextP) :: Nil
        case Nil => Nil
      }
    }

    private def moveAlongEdge(paramEdge: ParametrizedLine, optimim: OptFun): Double = {
      def optimumFun(t: Double): Double = { optimim(paramEdge(t)) }
      new OptimumParametrisedSearch(optimumFun).find
    }

    private def getInitEdge(from: Point): MyVector = {
      if (figure.vertex.contains(from)) {
        figure.vertexToLines(from).right
      } else {
        figure.lines.find(vector => vector.containsAsRectangle(from) && vector.contains(from))
          .getOrElse(throw new IllegalArgumentException("Point not on edge of figure"))
      }
    }
  }
}

