package geometry

import geometry.support.GeometrySupport

/**
  * Created by k.neyman on 20.11.2017. 
  */
case class Figure(lines: List[MyVector], vertices: List[Point]) extends GeometrySupport {
  def this(lines: List[MyVector]) = this(lines.map(_.nonBounded), lines.map(_.from))

  case class PointNeighbors(left: MyVector, right: MyVector)

  val vertexToLines: Map[Point, PointNeighbors] = {
    val shifted = lines.tail :+ lines.head
    lines.zip(shifted).map { case (left, right) =>
      left.to -> PointNeighbors(left, right)
    }.toMap
  }

  private def xs = lines.toStream.flatMap(line => line.from.x :: line.to.x :: Nil).distinct
  private lazy val maxX = xs.max

  def containsPoint(p: Point): Boolean = {
    if (p.x > maxX || vertices.contains(p)) false
    else {
      implicit val checkVector = MyVector(p, p.copy(x = maxX + 1))
      val lineCross = lines.toStream.count(_.isCrossedBy(checkVector))
      val vertexCross = vertices.count(isInnerCross)
      (lineCross + vertexCross) % 2 == 1
    }
  }

  def isInnerCross(point: Point)(implicit checkVector: MyVector): Boolean = {
    if (checkVector.from.y == point.y && checkVector.from.x <= point.x) {
      val neighbors: PointNeighbors = vertexToLines(point)
      neighbors.left.y * neighbors.right.y > 0
    } else {
      false
    }
  }
}

object Figure {
  def fromVertices(points: List[Point]): Figure = Figure(
    (points zip (points.drop(1) :+ points.head)).map { case (a, b) => MyVector(a, b).nonBounded },
    points)
}