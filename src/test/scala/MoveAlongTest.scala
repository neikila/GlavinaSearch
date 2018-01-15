import geometry._
import geometry.support.GeometrySupport
import labtask.{AccuracySettings, Algo, Field}
import labtask.Algo.Result
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class MoveAlongTest extends FunSuite with GeometrySupport {
  private implicit val accuracySettings: AccuracySettings = new AccuracySettings
  implicit val accuracy: EPS = 0.1

  private val points = Point(2, 2) :: Point(2, 5) :: Point(6, 5) :: Point(6, 2) :: Nil
  test("Move along box finish at vertex") {
    val barrier = Figure.fromVertices(points)
    val target = Point(6, 7)
    def distanceToTarget(p: Point): Double = MyVector(p, target).length2

    val startPoint = Point(3.5, 2)
    val result = barrier.moveAlongToTarget(startPoint, distanceToTarget, Nil)
    val ideal = List(MyVector(startPoint, Point(6, 2)), MyVector(Point(6, 2), Point(6, 5)))

    assertResult(result, ideal)
  }

  test("Move along box finish at middle") {
    val barrier = Figure.fromVertices(points)
    val target = Point(9, 4)
    def distanceToTarget(p: Point): Double = MyVector(p, target).length2

    val startPoint = Point(5, 2)
    val result = barrier.moveAlongToTarget(startPoint, distanceToTarget, Nil)
    val ideal = List(MyVector(startPoint, Point(6, 2)), MyVector(Point(6, 2), Point(6, 4)))

    assertResult(result, ideal)
  }

  test("Move along box stop before another box") {
    val barrier = Figure.fromVertices(points)
    val barrier2 = Figure.fromVertices(points.map { case Point(x, y) => Point(x + 3, y - 1)})

    val target = Point(12, 4)
    def distanceToTarget(p: Point): Double = MyVector(p, target).length2

    val startPoint = Point(3, 2)
    val result = barrier.moveAlongToTarget(startPoint, distanceToTarget, barrier2 :: Nil)
    val ideal = List(MyVector(startPoint, Point(5, 2)))

    assertResult(result, ideal)
  }

  test("Move along box stop before another box start at vertex") {
    val barrier = Figure.fromVertices(points)
    val barrier2 = Figure.fromVertices(points.map { case Point(x, y) => Point(x + 3, y - 1)})

    val target = Point(7, 6)
    def distanceToTarget(p: Point): Double = MyVector(p, target).length2

    val startPoint = Point(2, 2)
    val result = barrier.moveAlongToTarget(startPoint, distanceToTarget, barrier2 :: Nil)
    val ideal = List(MyVector(startPoint, Point(2, 5)), MyVector(Point(2, 5), Point(6, 5)))

    assertResult(result, ideal)
  }

  private def assertResult(actual: List[MyVector], ideal: List[MyVector]): Unit = {
    assert(comparePath(actual, ideal), s"\nresult: $actual \n ideal: $ideal")
  }

  private def toPoints(vectors: List[MyVector]): List[Point] = vectors.head.from :: vectors.map(_.to)

  type EPS = Double
  private def comparePath(actual: Result, ideal: Result)(implicit rangeToPoint: EPS): Boolean = {
    val range2 = math.pow(rangeToPoint, 2)
    def inRange(p1: Point, p2: Point): Boolean = MyVector(p1, p2).length2 < range2
    actual.size == ideal.size && (toPoints(actual) zip toPoints(ideal)).forall { case (p1, p2) => inRange(p1, p2) }
  }
}
