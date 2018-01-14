package geometry.support

import geometry.Point.PointAccuracyEqual
import geometry._
import geometry.task.AccuracySettings
//import geometry.support.FigureInterceptSupport.BoundaryPointDetectionAccuracy

/**
  * Created by Neikila on 14.01.2018.
  */
trait FigureInterceptSupport extends LineCrossSupport with ParametrizedLineSupport {
  implicit class FigureLineInterception(val figure: Figure)(implicit val accuracySettings: AccuracySettings) {
    private implicit val pointAccuracyEqual: PointAccuracyEqual = accuracySettings.POINT_ACCURACY_EQUAL
    private val eps2: MyVectorAccuracy.ContainsAccuracy = accuracySettings.VECTOR_CONTAIN_ACCURACY

    def findCrossings(vector: MyVector): Iterable[Point] = {
      val withLines = findCrossingsWithLines(vector)
      val withVertex = findCrossingWithVertex(vector)
      withVertex ::: withLines
    }

    def findCrossingsWithLines(vector: MyVector): List[Point] = {
      figure.lines.flatMap(line => findCrossingWithLine(vector, line))
    }

    private def findCrossingWithLine(vector: MyVector, line: MyVector): Option[Point] = {
      line.findCrossing(vector).map { p => line.findProjection(p) } match {
        case optP: Some[Point] => optP.filter(checkPointForBoundaryCondtions(_, vector))
        case _ => None
      }
    }

    def findCrossingWithVertex(vector: MyVector): List[Point] = {
      figure.vertices.filter(p => vector.contains(p)(eps2) && checkPointForBoundaryCondtions(p, vector))
    }

    private def checkPointForBoundaryCondtions(p: Point, vector: MyVector): Boolean = {
      if (vector.from.isApproximatelyEqual(p)) isCrossingFromOutside(vector)
      else if (vector.to.isApproximatelyEqual(p)) isCrossingFromInside(vector)
      else true
    }

    private def isCrossingFromOutside(vector: MyVector): Boolean = {
      figure.containsPoint(new ParametrizedLine(vector).apply(accuracySettings.EPS_ACCURACY_CROSS_DETECTION_AT_BOUND / vector.length))
    }

    private def isCrossingFromInside(vector: MyVector): Boolean = {
      figure.containsPoint(new ParametrizedLine(vector).apply(1 - accuracySettings.EPS_ACCURACY_CROSS_DETECTION_AT_BOUND))
    }
  }
}

object FigureInterceptAccuracy {
  type BoundaryPointDetectionAccuracy = Double
}

