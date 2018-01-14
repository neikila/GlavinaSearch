package geometry

/**
  * Created by Neikila on 14.01.2018.
  */
trait LineCrossSupport {
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
}
