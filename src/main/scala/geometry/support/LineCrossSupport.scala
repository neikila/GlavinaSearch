package geometry.support

import geometry.{Line, MyVector, Point}

/**
  * Created by Neikila on 14.01.2018.
  */
trait LineCrossSupport {
  implicit class LineCrossDetector(val line: Line) {

    private def determinant(a1: Double, b1: Double, a2: Double, b2: Double): Double = {
      a1 * b2 - a2 * b1
    }

    def findCrossing(anotherLine: Line): Option[Point] = {
      if (line == anotherLine) {
        throw new IllegalArgumentException("Lines are parallel")
      }

      val D = determinant(line.a, line.b, anotherLine.a, anotherLine.b)
      def Dx = determinant(-line.c, line.b, -anotherLine.c, anotherLine.b)
      def Dy = determinant(line.a, -line.c, anotherLine.a, -anotherLine.c)

      if (D == 0) None
      else Some(Point(Dx / D, Dy / D))
    }

    def isCrossedBy(anotherLine: Line): Boolean = {
      findCrossing(anotherLine).isDefined
    }
  }

  implicit class VectorCrossDetector(val vector: MyVector) {
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
      new LineCrossDetector(vector.line).findCrossing(anotherVector.line)
        .filter(p => anotherVector.contains(p) && vector.contains(p))
    }

    def isCrossedBy(anotherVector: MyVector): Boolean = {
      findCrossing(anotherVector).isDefined
    }
  }
}
