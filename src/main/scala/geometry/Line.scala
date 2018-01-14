package geometry

import geometry.Line.LineAccuracySearch

/**
  * Created by k.neyman on 20.11.2017. 
  */
case class Line(a: Double, b: Double, c: Double) {
  def distance2(point: Point): Double = {
    math.pow(a * point.x + b * point.y + c, 2) / (a * a + b * b)
  }

  def distance(point: Point): Double = math.sqrt(distance2(point))

  def contains(point: Point)(implicit accuracy: LineAccuracySearch) : Boolean = {
    distance2(point) < accuracy * accuracy
  }
}

object Line {
  type LineAccuracySearch = Double
}