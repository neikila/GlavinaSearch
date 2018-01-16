package geometry

import geometry.support.LineCrossSupport
import labtask.AccuracySettings

/**
  * Created by k.neyman on 20.11.2017. 
  */
case class Figure(lines: List[MyVector], vertices: List[Point]) extends LineCrossSupport {
  private implicit val accuracy: MyVectorAccuracy.ContainsAccuracy = new AccuracySettings().VECTOR_CONTAIN_ACCURACY

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

  override def equals(other: Any): Boolean = other match {
    case that: Figure => vertices == that.vertices
    case _ => false
  }
}

object Figure {
  def fromVertices(points: List[Point]): Figure = Figure(
    (points zip (points.drop(1) :+ points.head)).map { case (a, b) => MyVector(a, b).nonBounded },
    points)

  def rect(point1: Point, point2: Point): Figure =
    Figure.fromVertices(List(point1, point1.copy(y = point2.y), point2, point2.copy(y = point1.y)))
}