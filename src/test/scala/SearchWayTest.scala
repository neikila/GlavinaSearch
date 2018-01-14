import Algo.Result
import geometry.support.GeometrySupport
import geometry.{Figure, MyVector, Point}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class SearchWayTest extends FunSuite with GeometrySupport {

  implicit val accuracy: EPS = 0.1

  private def makePath(points: List[Point]): List[MyVector] = {
    (points zip points.tail).map { case (p1, p2) => MyVector(p1, p2) }
  }

  private def toPoints(vectors: List[MyVector]): List[Point] = vectors.head.from :: vectors.map(_.to)

  type EPS = Double
  private def comparePath(actual: Result, ideal: Result)(implicit rangeToPoint: EPS): Boolean = {
    val range2 = math.pow(rangeToPoint, 2)
    def inRange(p1: Point, p2: Point): Boolean = MyVector(p1, p2).length2 < range2
    if (actual.size == ideal.size) {
      if (actual.isEmpty) true
      else {
        val zipped = toPoints(actual) zip toPoints(ideal)
        zipped.forall { case (p1, p2) => inRange(p1, p2) }
      }
    } else {
      false
    }
  }

  test("testSimpleWay") {
    val barrier = Figure.fromVertices(Point(2, 2) :: Point(2, 5) :: Point(6, 5) :: Point(6, 2) :: Nil)

    val result: Result = new Algo(Field(100, 100), barrier :: Nil, Point(3, 1), Point(6, 7)).solve()
    val ideal = List(
      MyVector(Point(3, 1), Point(3.5, 2)),
      MyVector(Point(3.5, 2), Point(6, 2)),
      MyVector(Point(6, 2), Point(6, 5)),
      MyVector(Point(6, 5), Point(6, 7)))
    assert(comparePath(result, ideal), s"result: $result \n ideal: $ideal")
  }

  test("testSimpleWay2") {
    val barrier = Figure.fromVertices(Point(2, 2) :: Point(2, 5) :: Point(6, 5) :: Point(6, 2) :: Nil)

    val result: Result = new Algo(Field(100, 100), barrier :: Nil, Point(3, 1), Point(9, 4)).solve()
    val ideal = List(
      MyVector(Point(3, 1), Point(5, 2)),
      MyVector(Point(5, 2), Point(6, 2)),
      MyVector(Point(6, 2), Point(6, 4)),
      MyVector(Point(6, 4), Point(9, 4)))
    assert(comparePath(result, ideal), s"result: $result \n ideal: $ideal")
  }

  test("testSimpleWayBadNums") {
    val barrier = Figure.fromVertices(Point(2, 2) :: Point(2, 5) :: Point(6, 5) :: Point(6, 2) :: Nil)

    val result: Result = new Algo(Field(100, 100), barrier :: Nil, Point(3, 1), Point(7, 4)).solve()
    val ideal = makePath(List(Point(3, 1), Point(4 + (1.0 / 3), 2), Point(6, 2), Point(6, 4), Point(7, 4)))
    println(toPoints(result))
    assert(comparePath(result, ideal), s"result: $result \n ideal: $ideal")
  }

  test("testSearchWithDeadEnd") {
    val root = Point(2, 1)
    val finish = Point(14, 9)

    val barrier1 = Figure.fromVertices(Point(3, 2) :: Point(3, 5) :: Point(8, 5) :: Point(8, 2) :: Nil)
    val barrier2 = Figure.fromVertices(Point(7, 6) :: Point(7, 8) :: Point(13, 8) :: Point(13, 4) :: Point(11, 4) :: Point(11, 6) :: Nil)

    val result: Result = new Algo(Field(100, 100), barrier1 :: barrier2 :: Nil, root, finish).solve()
    val ideal =  makePath(List(root, Point(3.5, 2), Point(8, 2), Point(8, 5), Point(9.5, 6), Point(11, 6)))

    println(toPoints(result))
    assert(comparePath(result, ideal), s"result: $result \n ideal: $ideal")
  }
}
