package geometry

import geometry.Point.PointAccuracyEqual

/**
  * Created by k.neyman on 20.11.2017. 
  */
case class Point(x: Double, y: Double) {
  def *(point: Point): Double = x * point.x + y * point.y
  def isApproximatelyEqual(point: Point)(implicit accuracy: PointAccuracyEqual): Boolean =
    MyVector(this, point).length2 < accuracy * accuracy
}

object Point {
  type PointAccuracyEqual = Double
}