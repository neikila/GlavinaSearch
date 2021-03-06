import geometry.support.GeometrySupport
import labtask.{Algo, Field}
import geometry.{Figure, MyVector, Point}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class SearchWayTest extends FunSuite with GeometrySupport {
  import Algo.Result
  implicit val accuracy: EPS = 0.1

  private def makePath(points: List[Point]): List[MyVector] = {
    (points zip points.tail).map { case (p1, p2) => MyVector(p1, p2) }
  }

  private def toPoints(vectors: List[MyVector]): List[Point] = vectors.head.from :: vectors.map(_.to)

  type EPS = Double
  private def comparePath(actual: Result, ideal: Result)(implicit rangeToPoint: EPS): Boolean = {
    val range2 = math.pow(rangeToPoint, 2)
    def inRange(p1: Point, p2: Point): Boolean = MyVector(p1, p2).length2 < range2
    actual.size == ideal.size && (toPoints(actual) zip toPoints(ideal)).forall { case (p1, p2) => inRange(p1, p2) }
  }

  test("Simple way around box") {
    val barrier = Figure.fromVertices(Point(2, 2) :: Point(2, 5) :: Point(6, 5) :: Point(6, 2) :: Nil)

    val result: Result = new Algo(Field(100, 100), barrier :: Nil, Point(3, 1), Point(6, 7)).solve()
    val ideal = List(
      MyVector(Point(3, 1), Point(3.5, 2)),
      MyVector(Point(3.5, 2), Point(6, 2)),
      MyVector(Point(6, 2), Point(6, 5)),
      MyVector(Point(6, 5), Point(6, 7)))

    assertResult(result, ideal)
  }

  test("Simple way around box, Finish Move along at Middle of line") {
    val barrier = Figure.fromVertices(Point(2, 2) :: Point(2, 5) :: Point(6, 5) :: Point(6, 2) :: Nil)

    val result: Result = new Algo(Field(100, 100), barrier :: Nil, Point(3, 1), Point(9, 4)).solve()
    val ideal = List(
      MyVector(Point(3, 1), Point(5, 2)),
      MyVector(Point(5, 2), Point(6, 2)),
      MyVector(Point(6, 2), Point(6, 4)),
      MyVector(Point(6, 4), Point(9, 4)))

    assertResult(result, ideal)
  }

  test("Simple way uncomfortable nums") {
    val barrier = Figure.fromVertices(Point(2, 2) :: Point(2, 5) :: Point(6, 5) :: Point(6, 2) :: Nil)

    val result: Result = new Algo(Field(100, 100), barrier :: Nil, Point(3, 1), Point(7, 4)).solve()
    val ideal = makePath(List(Point(3, 1), Point(4 + (1.0 / 3), 2), Point(6, 2), Point(6, 4), Point(7, 4)))

    assertResult(result, ideal)
  }

//  test("Search box and L-figure with dead end") {
//    val root = Point(2, 1)
//    val finish = Point(14, 9)
//
//    val barrier1 = Figure.fromVertices(Point(3, 2) :: Point(3, 5) :: Point(8, 5) :: Point(8, 2) :: Nil)
//    val barrier2 = Figure.fromVertices(Point(7, 6) :: Point(7, 8) :: Point(13, 8) :: Point(13, 4) :: Point(11, 4) :: Point(11, 6) :: Nil)
//
//    val result: Result = new Algo(Field(100, 100), barrier1 :: barrier2 :: Nil, root, finish).solve()
//    val ideal = makePath(List(root, Point(3.5, 2), Point(8, 2), Point(8, 5), Point(9.5, 6), Point(11, 6)))
//    assertResult(result, ideal)
//  }
//
  def assertResult(actual: List[MyVector], ideal: List[MyVector]): Unit = {
    assert(comparePath(actual, ideal), s"\nresult: $actual \n ideal: $ideal")
  }
}
