import geometry._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.language.postfixOps


@RunWith(classOf[JUnitRunner])
class FigureSupport extends FunSuite with GeometrySupport {
  private def createRhombus: Figure = {
    val points = Point(0, 5) :: Point(7.5, 10) :: Point(15, 5) :: Point(7.5, 0) :: Nil
    Figure.fromVertex(points)
  }

  test("testContains") {
    assert(createRhombus.containsPoint(Point(7.5, 4)))
  }

  test("testParallel") {
    assert(createRhombus.containsPoint(Point(7.5, 5)))
  }

  test("testNotContains") {
    assert(!createRhombus.containsPoint(Point(7.5, 15)))
  }

  test("testNotContainsHorizontal") {
    assert(createRhombus.vertex.forall(p => !createRhombus.containsPoint(p)))
  }

  test("Crossed By Vector") {
    val box: Figure = Figure.fromVertex(Point(2, 2) :: Point(2, 4) :: Point(4, 4) :: Point(4, 2) :: Nil)

    assert(box.findCrossings(MyVector(Point(3, 3), Point(2, 3))) == Point(2, 3) :: Nil)
    assert(box.findCrossings(MyVector(Point(2, 3), Point(3, 3))) == Point(2, 3) :: Nil)
    assert(box.findCrossings(MyVector(Point(1, 3), Point(2, 3))) == Nil)
    assert(box.findCrossings(MyVector(Point(2, 3), Point(1, 3))) == Nil)

    assert(box.findCrossings(MyVector(Point(2, 2), Point(3, 3))) == Point(2, 2) :: Nil)
    assert(box.findCrossings(MyVector(Point(3, 3), Point(2, 2))) == Point(2, 2) :: Nil)
    assert(box.findCrossings(MyVector(Point(1, 1), Point(2, 2))) == Nil)
    assert(box.findCrossings(MyVector(Point(2, 2), Point(1, 1))) == Nil)
  }

  test("testCrossedByVector2") {
    val root = Point(11, 6)
    val finish = Point(14, 9)

    val figure = Figure.fromVertex(Point(7, 6) :: Point(7, 8) :: Point(13, 8) :: Point(13, 4) :: Point(11, 4) :: Point(11, 6) :: Nil)
    val vector: MyVector = MyVector(root, finish)
    //      println(barrier2.findCrossings(vector))

    val p = root
    val r1 = vector.contains(p) && vector.containsAsRectangle(p)
    //      println(r1)
    val EPS_ACCURACY_CROSS_DETECTION_AT_BOUND = 0.001
    //      println(new FigureLineInterception(figure).checkPoint(p, vector))
    val p2: Point = new ParametrizedLine(vector).apply(EPS_ACCURACY_CROSS_DETECTION_AT_BOUND)
//    assert(p2)
    assert(figure.containsPoint(p2))
  }
}
