package geometry

/**
  * Created by Neikila on 14.01.2018.
  */
trait FigureLineInterceptSupport extends LineCrossSupport {
  implicit class FigureLineInterception(val figure: Figure) {
    private val EPS_ACCURACY_CROSS_DETECTION_AT_BOUND: Double = 0.1

    def findCrossings(vector: MyVector): Iterable[Point] = {
      (findCrossingWithVertex(vector) ::: findCrossingsWithLines(vector)).distinct
    }

    def findCrossingsWithLines(vector: MyVector): List[Point] = {
      figure.lines.flatMap(line => findCrossingWithLine(vector, line))
    }

    private def findCrossingWithLine(vector: MyVector, line: MyVector): Option[Point] = {
      line.findCrossing(vector) match {
        case optP: Some[Point] => optP.filter(checkPoint(_, vector))
        case _ => None
      }
    }

    def findCrossingWithVertex(vector: MyVector): List[Point] = {
      figure.vertices.filter(p =>
        vector.contains(p) &&
          vector.containsAsRectangle(p) &&
          checkPoint(p, vector))
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
}
